module Beckn.Product.MapSearch.GoogleMaps
  ( GetDistanceResult (..),
    GetDistanceResultInfo (..),
    getDistance,
    getDistances,
    getRoutes,
    HasCoordinates (..),
    getDistancesGeneral,
  )
where

import qualified Beckn.External.GoogleMaps.Client as GoogleMaps
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common hiding (id)
import qualified Data.List.NonEmpty as NE
import GHC.Float

data GetDistanceResult = GetDistanceResult
  { origin :: GoogleMaps.Place,
    destination :: GoogleMaps.Place,
    info :: GetDistanceResultInfo
  }
  deriving (Generic)

data GetDistanceResultInfo = GetDistanceResultInfo
  { distance :: Meter,
    duration :: NominalDiffTime,
    duration_in_traffic :: NominalDiffTime,
    status :: Text
  }
  deriving (Generic)

getDistance ::
  ( MonadFlow m,
    CoreMetrics m,
    GoogleMaps.HasGoogleMaps m r
  ) =>
  Maybe MapSearch.TravelMode ->
  MapSearch.LatLong ->
  MapSearch.LatLong ->
  Maybe UTCTime ->
  m GetDistanceResult
getDistance travelMode origin destination utcDepartureTime =
  getDistances travelMode (origin :| []) (destination :| []) utcDepartureTime >>= \case
    [] -> throwError (InternalError "Empty GoogleMaps.getDistance result.")
    [a] -> return a
    _ -> throwError (InternalError "Exactly one GoogleMaps.getDistance result expected.")

getDistances ::
  ( MonadFlow m,
    CoreMetrics m,
    GoogleMaps.HasGoogleMaps m r
  ) =>
  Maybe MapSearch.TravelMode ->
  NonEmpty MapSearch.LatLong ->
  NonEmpty MapSearch.LatLong ->
  Maybe UTCTime ->
  m [GetDistanceResult]
getDistances travelMode origins destinations =
  getDistancesGeneral travelMode origins destinations $
    \o d i -> GetDistanceResult (latLongToPlace o) (latLongToPlace d) i

--
class HasCoordinates a where
  getCoordinates :: a -> MapSearch.LatLong

instance HasCoordinates MapSearch.LatLong where
  getCoordinates = identity

--
getDistancesGeneral ::
  ( MonadFlow m,
    CoreMetrics m,
    GoogleMaps.HasGoogleMaps m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Maybe MapSearch.TravelMode ->
  NonEmpty a ->
  NonEmpty b ->
  (a -> b -> GetDistanceResultInfo -> c) ->
  Maybe UTCTime ->
  m [c]
getDistancesGeneral travelMode origins destinations zipFunc utcDepartureTime = do
  googleMapsUrl <- asks (.googleMapsUrl)
  key <- asks (.googleMapsKey)
  limitedOriginObjects <- capListAsPerGoogleLimits "origins" origins
  limitedDestinationObjects <- capListAsPerGoogleLimits "destinations" destinations
  let limitedOriginPlaces = map (latLongToPlace . getCoordinates) limitedOriginObjects
      limitedDestinationPlaces = map (latLongToPlace . getCoordinates) limitedDestinationObjects
  let departureTime = case utcDepartureTime of
        Nothing -> Just GoogleMaps.Now
        Just time -> Just $ GoogleMaps.FutureTime time
  GoogleMaps.distanceMatrix googleMapsUrl limitedOriginPlaces limitedDestinationPlaces key departureTime mode
    >>= parseDistanceMatrixRespGeneral zipFunc (toList limitedOriginObjects) (toList limitedDestinationObjects)
  where
    mode = mapToMode <$> travelMode

    -- Constraints on Distance matrix API: https://developers.google.com/maps/documentation/distance-matrix/usage-and-billing#other-usage-limits
    capListAsPerGoogleLimits listName inputList = do
      when (length inputList > 25) $
        logWarning ("Capping " <> listName <> " to maximum 25 elements as per Distance matrix API limits")
      return $ take 25 $ toList inputList

getRoutes ::
  ( MonadFlow m,
    CoreMetrics m,
    GoogleMaps.HasGoogleMaps m r
  ) =>
  MapSearch.Request ->
  m GoogleMaps.DirectionsResp
getRoutes req = do
  googleMapsUrl <- asks (.googleMapsUrl)
  key <- asks (.googleMapsKey)
  let origin = latLongToPlace (NE.head req.waypoints)
      destination = latLongToPlace (NE.last req.waypoints)
      waypoints = getWayPoints req.waypoints
      mode = mapToMode <$> req.mode
  GoogleMaps.directions googleMapsUrl origin destination key mode waypoints
  where
    getWayPoints waypoints =
      case NE.tail waypoints of
        [] -> Nothing
        _ -> Just (map latLongToPlace (init $ NE.tail waypoints))

parseDistanceMatrixRespGeneral ::
  (MonadThrow m, MonadIO m, Log m) =>
  (a -> b -> GetDistanceResultInfo -> c) ->
  [a] ->
  [b] ->
  GoogleMaps.DistanceMatrixResp ->
  m [c]
parseDistanceMatrixRespGeneral zipFunc origins destinations distanceMatrixResp = do
  mapM buildGetDistanceResult origDestAndElemList
  where
    origDestAndElemList = do
      (orig, row) <- zip origins distanceMatrixResp.rows
      (dest, element) <- zip destinations row.elements
      return (orig, dest, element)

    buildGetDistanceResultInfo element = do
      void $ GoogleMaps.validateResponseStatus element
      distance <- parseDistances element
      duration <- parseDuration element
      durationInTraffic <- parseDurationInTraffic element
      return $ GetDistanceResultInfo distance duration durationInTraffic element.status

    buildGetDistanceResult (orig, dest, element) = do
      info <- buildGetDistanceResultInfo element
      pure $ zipFunc orig dest info

latLongToPlace :: MapSearch.LatLong -> GoogleMaps.Place
latLongToPlace MapSearch.LatLong {..} =
  GoogleMaps.Location $ GoogleMaps.LocationS {lat = lat, lng = lon}

mapToMode :: MapSearch.TravelMode -> GoogleMaps.Mode
mapToMode MapSearch.CAR = GoogleMaps.DRIVING
mapToMode MapSearch.MOTORCYCLE = GoogleMaps.DRIVING
mapToMode MapSearch.BICYCLE = GoogleMaps.BICYCLING
mapToMode MapSearch.FOOT = GoogleMaps.WALKING

parseDistances :: (MonadThrow m, Log m) => GoogleMaps.DistanceMatrixElement -> m Meter
parseDistances distanceMatrixElement = do
  distance <-
    distanceMatrixElement.distance
      & fromMaybeM (InternalError "No distance value provided in distance matrix API response")
  pure $ Meter $ int2Double distance.value

parseDuration :: (MonadThrow m, Log m) => GoogleMaps.DistanceMatrixElement -> m NominalDiffTime
parseDuration distanceMatrixElement = do
  durationInTraffic <-
    distanceMatrixElement.duration
      & fromMaybeM (InternalError "No duration value provided in distance matrix API response")
  pure $ intToNominalDiffTime durationInTraffic.value

parseDurationInTraffic :: (MonadThrow m, Log m) => GoogleMaps.DistanceMatrixElement -> m NominalDiffTime
parseDurationInTraffic distanceMatrixElement = do
  durationInTraffic <-
    distanceMatrixElement.duration_in_traffic
      & fromMaybeM (InternalError "No duration_in_traffic value provided in distance matrix API response")
  pure $ intToNominalDiffTime durationInTraffic.value
