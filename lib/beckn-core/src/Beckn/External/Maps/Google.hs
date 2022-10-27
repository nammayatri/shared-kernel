module Beckn.External.Maps.Google
  ( module Reexport,
    getDistance,
    getDistances,
    getRoutes,
    snapToRoad,
    autoComplete,
    placeDetails,
    getPlaceName,
    distanceMatrix,
  )
where

import Beckn.External.Maps.Google.Config as Reexport
import qualified Beckn.External.Maps.Google.MapsClient as GoogleMaps
import Beckn.External.Maps.Google.MapsClient.Types as Reexport
import Beckn.External.Maps.Google.PolyLinePoints
import Beckn.External.Maps.Google.RoadsClient as Reexport
  ( SnapToRoadResponse,
    SnapToRoadResponse' (..),
    SnappedPoint,
    SnappedPoint' (..),
  )
import qualified Beckn.External.Maps.Google.RoadsClient as GoogleRoads
import Beckn.External.Maps.HasCoordinates as Reexport (HasCoordinates (..))
import Beckn.External.Maps.Types as Reexport
import qualified Beckn.External.Maps.Types as MapSearch
import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import Beckn.Utils.Common hiding (id)
import Control.Monad.Extra (concatForM)
import qualified Data.List.Extra as List
import qualified Data.List.NonEmpty as NE
import GHC.Float (double2Int)

getDistance ::
  ( MonadFlow m,
    CoreMetrics m,
    MonadReader r m,
    HasGoogleCfg r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Maybe MapSearch.TravelMode ->
  a ->
  b ->
  m (GetDistanceResult a b)
getDistance travelMode origin destination =
  getDistances travelMode (origin :| []) (destination :| []) >>= \case
    (a :| []) -> return a
    _ -> throwError (InternalError "Exactly one GoogleMaps.getDistance result expected.")

getDistances ::
  ( MonadFlow m,
    CoreMetrics m,
    MonadReader r m,
    HasGoogleCfg r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Maybe MapSearch.TravelMode ->
  NonEmpty a ->
  NonEmpty b ->
  m (NonEmpty (GetDistanceResult a b))
getDistances travelMode origins destinations = do
  googleMapsUrl <- asks (.googleCfg.googleMapsUrl)
  key <- asks (.googleCfg.googleKey)
  let limitedOriginObjectsList = splitListByAPICap origins
      limitedDestinationObjectsList = splitListByAPICap destinations
  res <- concatForM limitedOriginObjectsList $ \limitedOriginObjects ->
    concatForM limitedDestinationObjectsList $ \limitedDestinationObjects -> do
      let limitedOriginPlaces = map (latLongToPlace . getCoordinates) limitedOriginObjects
          limitedDestinationPlaces = map (latLongToPlace . getCoordinates) limitedDestinationObjects
      GoogleMaps.distanceMatrix googleMapsUrl key limitedOriginPlaces limitedDestinationPlaces mode
        >>= parseDistanceMatrixResp limitedOriginObjects limitedDestinationObjects
  case res of
    [] -> throwError (InternalError "Empty GoogleMaps.getDistances result.")
    (a : xs) -> return $ a :| xs
  where
    mode = mapToMode <$> travelMode

    -- Constraints on Distance matrix API: https://developers.google.com/maps/documentation/distance-matrix/usage-and-billing#other-usage-limits
    splitListByAPICap inputList = do
      List.chunksOf 25 $ toList inputList

getRoutes ::
  ( MonadFlow m,
    CoreMetrics m,
    MonadReader r m,
    HasGoogleCfg r,
    Log m
  ) =>
  MapSearch.GetRoutesReq ->
  m MapSearch.GetRoutesResp
getRoutes req = do
  googleMapsUrl <- asks (.googleCfg.googleMapsUrl)
  key <- asks (.googleCfg.googleKey)
  let origin = latLongToPlace (NE.head req.waypoints)
      destination = latLongToPlace (NE.last req.waypoints)
      waypoints = getWayPoints req.waypoints
      mode = mapToMode <$> req.mode
  gRes <- GoogleMaps.directions googleMapsUrl key origin destination mode waypoints
  traverse (mkRoute req) gRes.routes
  where
    getWayPoints waypoints =
      case NE.tail waypoints of
        [] -> Nothing
        _ -> Just (map latLongToPlace (init $ NE.tail waypoints))

mkRoute ::
  (MonadFlow m) =>
  MapSearch.GetRoutesReq ->
  GoogleMaps.Route ->
  m MapSearch.RouteInfo
mkRoute req route = do
  let bound = Just $ mkBounds route.bounds
  if null route.legs
    then do
      logTagWarning "GoogleMapsDirections" ("Empty route.legs, " <> show req)
      return $ MapSearch.RouteInfo Nothing Nothing bound [] []
    else do
      when (length route.legs > 1) $
        logTagWarning "GoogleMapsDirections" ("More than one element in route.legs, " <> show req)

      let leg = head route.legs
          steps = leg.steps
          polylinePoints = concat $ (\step -> decode step.polyline.points) <$> steps
          snappedWayPoints = (\step -> (MapSearch.LatLong step.start_location.lat step.start_location.lng, MapSearch.LatLong step.end_location.lat step.end_location.lng)) <$> steps
          distanceInM = Just $ fromIntegral leg.distance.value
          durationInS = Just $ fromIntegral leg.duration.value

      return $ MapSearch.RouteInfo durationInS distanceInM bound snappedWayPoints polylinePoints
  where
    mkBounds :: GoogleMaps.Bounds -> MapSearch.BoundingBoxWithoutCRS
    mkBounds gBound =
      let ne = MapSearch.PointXY gBound.northeast.lat gBound.northeast.lng
          sw = MapSearch.PointXY gBound.southwest.lat gBound.southwest.lng
       in MapSearch.BoundingBoxWithoutCRSXY ne sw

parseDistanceMatrixResp ::
  (MonadThrow m, MonadIO m, Log m) =>
  [a] ->
  [b] ->
  GoogleMaps.DistanceMatrixResp ->
  m [GetDistanceResult a b]
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
        GetDistanceResult
          { origin = orig,
            destination = dest,
            distance = distance,
            duration = Seconds . double2Int . realToFrac $ duration,
            status = element.status
          }

latLongToPlace :: MapSearch.LatLong -> GoogleMaps.Place
latLongToPlace MapSearch.LatLong {..} =
  GoogleMaps.Location $ GoogleMaps.LocationS {lat = lat, lng = lon}

mapToMode :: MapSearch.TravelMode -> GoogleMaps.Mode
mapToMode MapSearch.CAR = GoogleMaps.DRIVING
mapToMode MapSearch.MOTORCYCLE = GoogleMaps.DRIVING
mapToMode MapSearch.BICYCLE = GoogleMaps.BICYCLING
mapToMode MapSearch.FOOT = GoogleMaps.WALKING

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
    CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasGoogleCfg r
  ) =>
  Bool ->
  [MapSearch.LatLong] ->
  m GoogleRoads.SnapToRoadResponse
snapToRoad interpolate pointsList = do
  roadsUrl <- asks (.googleCfg.googleRoadsUrl)
  apiKey <- asks (.googleCfg.googleKey)
  GoogleRoads.snapToRoad roadsUrl apiKey interpolate pointsList

autoComplete ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasGoogleCfg r
  ) =>
  Text ->
  Maybe Text ->
  Text ->
  Integer ->
  Text ->
  GoogleMaps.Language ->
  m GoogleMaps.SearchLocationResp
autoComplete input sessiontoken location radius components lang = do
  mapsUrl <- asks (.googleCfg.googleMapsUrl)
  apiKey <- asks (.googleCfg.googleKey)
  GoogleMaps.autoComplete mapsUrl apiKey input sessiontoken location radius components lang

placeDetails ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasGoogleCfg r
  ) =>
  Maybe Text ->
  Text ->
  Text ->
  m GoogleMaps.PlaceDetailsResp
placeDetails sessiontoken placeId fields = do
  mapsUrl <- asks (.googleCfg.googleMapsUrl)
  apiKey <- asks (.googleCfg.googleKey)
  GoogleMaps.placeDetails mapsUrl apiKey sessiontoken placeId fields

getPlaceName ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasGoogleCfg r
  ) =>
  Maybe Text ->
  GoogleMaps.GetPlaceNameBy ->
  Maybe GoogleMaps.Language ->
  m GoogleMaps.GetPlaceNameResp
getPlaceName sessiontoken by language = do
  mapsUrl <- asks (.googleCfg.googleMapsUrl)
  apiKey <- asks (.googleCfg.googleKey)
  GoogleMaps.getPlaceName mapsUrl apiKey sessiontoken by language

distanceMatrix ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasGoogleCfg r
  ) =>
  [GoogleMaps.Place] ->
  [GoogleMaps.Place] ->
  Maybe GoogleMaps.Mode ->
  m GoogleMaps.DistanceMatrixResp
distanceMatrix origins destinations mode = do
  mapsUrl <- asks (.googleCfg.googleMapsUrl)
  apiKey <- asks (.googleCfg.googleKey)
  GoogleMaps.distanceMatrix mapsUrl apiKey origins destinations mode
