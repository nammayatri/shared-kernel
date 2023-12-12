{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.Interface.Google
  ( module Reexport,
    getDistances,
    getRoutes,
    snapToRoad,
    autoComplete,
    getPlaceDetails,
    getPlaceName,
  )
where

import Control.Monad.Extra (concatForM)
import qualified Data.List.Extra as List
import qualified Data.List.NonEmpty as NE
import GHC.Float (double2Int)
import Kernel.External.Encryption
import Kernel.External.Maps.Google.Config as Reexport
import qualified Kernel.External.Maps.Google.MapsClient as GoogleMaps
import Kernel.External.Maps.Google.PolyLinePoints
import qualified Kernel.External.Maps.Google.RoadsClient as GoogleRoads
import Kernel.External.Maps.HasCoordinates as Reexport (HasCoordinates (..))
import Kernel.External.Maps.Interface.Types
import Kernel.External.Maps.Types as Reexport
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Utils.CalculateDistance (everySnippetIs, getRouteLinearLength)
import Kernel.Utils.Common hiding (id)

getDistancesWrapper ::
  ( EncFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  GetDistancesReq a b ->
  [[a]] ->
  [[b]] ->
  BaseUrl ->
  Text ->
  Maybe GoogleMaps.Mode ->
  Bool ->
  m [GetDistanceResp a b]
getDistancesWrapper _ limitedOriginObjectsList limitedDestinationObjectsList googleMapsUrl key mode isAvoidTolls = concatForM limitedOriginObjectsList $ \limitedOriginObjects ->
  concatForM limitedDestinationObjectsList $ \limitedDestinationObjects ->
    do
      let limitedOriginPlaces = map (latLongToPlace . getCoordinates) limitedOriginObjects
          limitedDestinationPlaces = map (latLongToPlace . getCoordinates) limitedDestinationObjects
      GoogleMaps.distanceMatrix googleMapsUrl key limitedOriginPlaces limitedDestinationPlaces mode isAvoidTolls
      >>= parseDistanceMatrixResp limitedOriginObjects limitedDestinationObjects

getDistances ::
  ( EncFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  GoogleCfg ->
  GetDistancesReq a b ->
  m (NonEmpty (GetDistanceResp a b))
getDistances cfg GetDistancesReq {..} = do
  let googleMapsUrl = cfg.googleMapsUrl
  key <- decrypt cfg.googleKey
  let limitedOriginObjectsList = splitListByAPICap origins
      limitedDestinationObjectsList = splitListByAPICap destinations
  res <- getDistancesWrapper GetDistancesReq {..} limitedOriginObjectsList limitedDestinationObjectsList googleMapsUrl key mode True
  case res of
    [] -> do
      logInfo "Falling back to avoid tolls"
      resp <- getDistancesWrapper GetDistancesReq {..} limitedOriginObjectsList limitedDestinationObjectsList googleMapsUrl key mode False
      case resp of
        [] -> throwError (InternalError "Empty GoogleMaps.getDistances result.")
        (a : xs) -> return $ a :| xs
    (a : xs) -> return $ a :| xs
  where
    mode = mapToMode <$> travelMode

    -- Constraints on Distance matrix API: https://developers.google.com/maps/documentation/distance-matrix/usage-and-billing#other-usage-limits
    splitListByAPICap inputList = do
      List.chunksOf 25 $ toList inputList

originAndDestinationRemover :: [a] -> [a]
originAndDestinationRemover waypoints = if length waypoints > 2 then init $ tail waypoints else []

routeToRouteProxyConverter :: GetRoutesReq -> GetRoutesReqProxy
routeToRouteProxyConverter req =
  GetRoutesReqProxy
    { origin = NE.head req.waypoints,
      destination = NE.last req.waypoints,
      mode = req.mode,
      calcPoints = req.calcPoints,
      waypoints = originAndDestinationRemover $ NE.toList req.waypoints
    }

getRoutes ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m
  ) =>
  Bool ->
  GoogleCfg ->
  GetRoutesReq ->
  m GetRoutesResp
getRoutes isAvoidToll cfg req = do
  let routeProxyReq = routeToRouteProxyConverter req
  let googleMapsUrl = cfg.googleMapsUrl
  key <- decrypt cfg.googleKey
  let origin = latLongToPlace routeProxyReq.origin
      destination = latLongToPlace routeProxyReq.destination
      waypoints = getWayPoints routeProxyReq.waypoints
      mode = mapToMode <$> routeProxyReq.mode
  gRes <- GoogleMaps.directions googleMapsUrl key origin destination mode waypoints isAvoidToll
  if null gRes.routes
    then do
      gResp <- GoogleMaps.directions googleMapsUrl key origin destination mode waypoints False
      traverse (mkRoute routeProxyReq) gResp.routes
    else traverse (mkRoute routeProxyReq) gRes.routes
  where
    getWayPoints waypoints =
      case waypoints of
        [] -> Nothing
        _ -> Just (map latLongToPlace waypoints)

mkRoute ::
  (MonadFlow m) =>
  GetRoutesReqProxy ->
  GoogleMaps.Route ->
  m RouteInfo
mkRoute req route = do
  let bound = Just $ mkBounds route.bounds
  if null route.legs
    then do
      logTagWarning "GoogleMapsDirections" ("Empty route.legs, " <> show req)
      return $ RouteInfo Nothing Nothing bound [] []
    else do
      when (length route.legs > 1) $
        logTagWarning "GoogleMapsDirections" ("More than one element in route.legs, " <> show req)

      let totalDistance = foldr (\leg acc -> acc + fromIntegral leg.distance.value) 0 route.legs
          totalDuration = foldr (\leg acc -> acc + fromIntegral leg.duration.value) 0 route.legs
          allSteps = foldr (\leg acc -> acc ++ leg.steps) [] route.legs
          polylinePoints = concatMap (\step -> decode step.polyline.points) allSteps
      -- TODO: Fix snappedWayPoints: the waypoint passed in request which are snapped to road
      -- snappedWayPoints = (\step -> (LatLong step.start_location.lat step.start_location.lng, LatLong step.end_location.lat step.end_location.lng)) <$> steps

      return $ RouteInfo (Just totalDuration) (Just totalDistance) bound [] polylinePoints
  where
    mkBounds :: GoogleMaps.Bounds -> BoundingBoxWithoutCRS
    mkBounds gBound =
      let ne = PointXY gBound.northeast.lat gBound.northeast.lng
          sw = PointXY gBound.southwest.lat gBound.southwest.lng
       in BoundingBoxWithoutCRSXY ne sw

parseDistanceMatrixResp ::
  (MonadThrow m, MonadIO m, Log m) =>
  [a] ->
  [b] ->
  GoogleMaps.DistanceMatrixResp ->
  m [GetDistanceResp a b]
parseDistanceMatrixResp origins destinations distanceMatrixResp = do
  mapM buildGetDistanceResult origDestAndElemList
  where
    origDestAndElemList = do
      (orig, row) <- zip origins distanceMatrixResp.rows
      (dest, element) <- zip destinations row.elements
      return (orig, dest, element)

    buildGetDistanceResult (orig, dest, element) = do
      distance <- parseDistances element
      duration <- parseDuration element
      pure $
        GetDistanceResp
          { origin = orig,
            destination = dest,
            distance = distance,
            duration = Seconds . double2Int . realToFrac $ duration,
            status = element.status
          }

latLongToPlace :: LatLong -> GoogleMaps.Place
latLongToPlace LatLong {..} =
  GoogleMaps.Location $ GoogleMaps.LocationS {lat = lat, lng = lon}

mapToMode :: TravelMode -> GoogleMaps.Mode
mapToMode CAR = GoogleMaps.DRIVING
mapToMode MOTORCYCLE = GoogleMaps.DRIVING
mapToMode BICYCLE = GoogleMaps.BICYCLING
mapToMode FOOT = GoogleMaps.WALKING

parseDistances :: (MonadThrow m, Log m) => GoogleMaps.DistanceMatrixElement -> m Meters
parseDistances distanceMatrixElement = do
  distance <-
    distanceMatrixElement.distance
      & fromMaybeM (InternalError "No distance value provided in distance matrix API response")
  pure $ Meters $ distance.value

parseDuration :: (MonadThrow m, Log m) => GoogleMaps.DistanceMatrixElement -> m NominalDiffTime
parseDuration distanceMatrixElement = do
  durationInTraffic <-
    distanceMatrixElement.duration
      & fromMaybeM (InternalError "No duration value provided in distance matrix API response")
  pure $ intToNominalDiffTime durationInTraffic.value

snapToRoad ::
  ( HasCallStack,
    EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["droppedPointsThreshold" ::: HighPrecMeters]
  ) =>
  GoogleCfg ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad cfg SnapToRoadReq {..} = do
  droppedPointsThreshold <- asks (.droppedPointsThreshold)
  unless (everySnippetIs (< droppedPointsThreshold) points) $ throwError (InternalError "Some snippets' length is above dropped points threshold")
  let roadsUrl = cfg.googleRoadsUrl
  key <- decrypt cfg.googleKey
  res <- GoogleRoads.snapToRoad roadsUrl key points
  let pts = map (.location) res.snappedPoints
  snippetThreshold <- asks (.snapToRoadSnippetThreshold)
  unless (everySnippetIs (< snippetThreshold) pts) $ throwError (InternalError "Some snippets' length is above threshold after snapToRoad")
  let dist = getRouteLinearLength pts
  pure
    SnapToRoadResp
      { distance = dist,
        confidence = 1.0, -- Considering Google's default confidence as 1.0
        snappedPoints = pts
      }

autoComplete ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  GoogleCfg ->
  AutoCompleteReq ->
  m AutoCompleteResp
autoComplete cfg AutoCompleteReq {..} = do
  let mapsUrl = cfg.googleMapsUrl
  key <- decrypt cfg.googleKey
  let components =
        case country of
          India -> "country:in"
          France -> "country:fr"
  res <- GoogleMaps.autoComplete mapsUrl key input sessionToken location radius components language strictbounds origin
  let predictions = map (\GoogleMaps.Prediction {..} -> Prediction {placeId = place_id, distance = distance_meters, ..}) res.predictions
  return $ AutoCompleteResp predictions

getPlaceDetails ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  GoogleCfg ->
  GetPlaceDetailsReq ->
  m GetPlaceDetailsResp
getPlaceDetails cfg GetPlaceDetailsReq {..} = do
  let mapsUrl = cfg.googleMapsUrl
  key <- decrypt cfg.googleKey
  let fields = "geometry"
  res <- GoogleMaps.getPlaceDetails mapsUrl key sessionToken placeId fields
  let location = let loc = res.result.geometry.location in LatLong loc.lat loc.lng
  return $ GetPlaceDetailsResp location

getPlaceName ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  GoogleCfg ->
  GetPlaceNameReq ->
  m GetPlaceNameResp
getPlaceName cfg GetPlaceNameReq {..} = do
  let mapsUrl = cfg.googleMapsUrl
  key <- decrypt cfg.googleKey
  res <- GoogleMaps.getPlaceName mapsUrl key sessionToken mbByPlaceId mbByLatLong language
  return $ map reformatePlaceName res.results
  where
    reformatePlaceName (placeName :: GoogleMaps.ResultsResp) =
      PlaceName
        { formattedAddress = placeName.formatted_address,
          addressComponents = map reformateAddressResp placeName.address_components,
          plusCode = placeName.plus_code <&> (.compound_code),
          location = let loc = placeName.geometry.location in LatLong loc.lat loc.lng,
          placeId = placeName.place_id
        }
    reformateAddressResp aResp =
      AddressResp
        { longName = aResp.long_name,
          shortName = aResp.short_name,
          types = aResp.types
        }
    (mbByPlaceId, mbByLatLong) = case getBy of
      ByPlaceId id -> (Just id, Nothing)
      ByLatLong latLong -> (Nothing, Just latLong)
