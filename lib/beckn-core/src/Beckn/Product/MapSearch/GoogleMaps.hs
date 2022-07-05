{-# LANGUAGE DefaultSignatures #-}

module Beckn.Product.MapSearch.GoogleMaps
  ( GetDistanceResult (..),
    getDistance,
    getDistances,
    getRoutes,
    HasCoordinates (..),
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
import Control.Monad.Extra (concatForM)
import qualified Data.List.Extra as List
import qualified Data.List.NonEmpty as NE
import GHC.Float (double2Int)

data GetDistanceResult a b = GetDistanceResult
  { origin :: a,
    destination :: b,
    distance :: Meters,
    duration :: Seconds,
    duration_in_traffic :: Seconds,
    status :: Text
  }
  deriving (Generic)

getDistance ::
  ( MonadFlow m,
    CoreMetrics m,
    GoogleMaps.HasGoogleMaps m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Maybe MapSearch.TravelMode ->
  a ->
  b ->
  Maybe UTCTime ->
  m (GetDistanceResult a b)
getDistance travelMode origin destination utcDepartureTime =
  getDistances travelMode (origin :| []) (destination :| []) utcDepartureTime >>= \case
    (a :| []) -> return a
    _ -> throwError (InternalError "Exactly one GoogleMaps.getDistance result expected.")

--
class HasCoordinates a where
  getCoordinates :: a -> MapSearch.LatLong
  default getCoordinates :: (HasField "lat" a Double, HasField "lon" a Double) => a -> MapSearch.LatLong
  getCoordinates = getCoordinatessDefault

getCoordinatessDefault :: (HasField "lat" a Double, HasField "lon" a Double) => a -> MapSearch.LatLong
getCoordinatessDefault loc = MapSearch.LatLong loc.lat loc.lon

instance HasCoordinates MapSearch.LatLong where
  getCoordinates = identity

--
getDistances ::
  ( MonadFlow m,
    CoreMetrics m,
    GoogleMaps.HasGoogleMaps m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Maybe MapSearch.TravelMode ->
  NonEmpty a ->
  NonEmpty b ->
  Maybe UTCTime ->
  m (NonEmpty (GetDistanceResult a b))
getDistances travelMode origins destinations utcDepartureTime = do
  googleMapsUrl <- asks (.googleMapsUrl)
  key <- asks (.googleMapsKey)
  let limitedOriginObjectsList = splitListByAPICap origins
      limitedDestinationObjectsList = splitListByAPICap destinations
  let departureTime = case utcDepartureTime of
        Nothing -> Just GoogleMaps.Now
        Just time -> Just $ GoogleMaps.FutureTime time
  res <- concatForM limitedOriginObjectsList $ \limitedOriginObjects ->
    concatForM limitedDestinationObjectsList $ \limitedDestinationObjects -> do
      let limitedOriginPlaces = map (latLongToPlace . getCoordinates) limitedOriginObjects
          limitedDestinationPlaces = map (latLongToPlace . getCoordinates) limitedDestinationObjects
      GoogleMaps.distanceMatrix googleMapsUrl limitedOriginPlaces limitedDestinationPlaces key departureTime mode
        >>= parseDistanceMatrixRespGeneral limitedOriginObjects limitedDestinationObjects
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
  [a] ->
  [b] ->
  GoogleMaps.DistanceMatrixResp ->
  m [GetDistanceResult a b]
parseDistanceMatrixRespGeneral origins destinations distanceMatrixResp = do
  mapM buildGetDistanceResult origDestAndElemList
  where
    origDestAndElemList = do
      (orig, row) <- zip origins distanceMatrixResp.rows
      (dest, element) <- zip destinations row.elements
      return (orig, dest, element)

    buildGetDistanceResult (orig, dest, element) = do
      void $ GoogleMaps.validateResponseStatus element
      distance <- parseDistances element
      duration <- parseDuration element
      durationInTraffic <- parseDurationInTraffic element
      pure $
        GetDistanceResult
          { origin = orig,
            destination = dest,
            distance = distance,
            duration = Seconds . double2Int . realToFrac $ duration,
            duration_in_traffic = Seconds . double2Int . realToFrac $ durationInTraffic,
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

parseDurationInTraffic :: (MonadThrow m, Log m) => GoogleMaps.DistanceMatrixElement -> m NominalDiffTime
parseDurationInTraffic distanceMatrixElement = do
  durationInTraffic <-
    distanceMatrixElement.duration_in_traffic
      & fromMaybeM (InternalError "No duration_in_traffic value provided in distance matrix API response")
  pure $ intToNominalDiffTime durationInTraffic.value
