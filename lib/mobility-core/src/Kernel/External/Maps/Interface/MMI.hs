{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.Interface.MMI
  ( autoSuggest,
    getDistanceMatrix,
    getRoutes,
    snapToRoad,
    reverseGeocode,
    getPlaceDetails,
    geocode,
  )
where

import Data.List.Extra ((!!))
import qualified Data.List.Extra as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import EulerHS.Prelude
import GHC.Float (double2Int)
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Google.PolyLinePoints as PP
import Kernel.External.Maps.HasCoordinates (HasCoordinates (..))
import Kernel.External.Maps.Interface.Types as IT
import Kernel.External.Maps.MMI.AutoSuggest as MMI
import Kernel.External.Maps.MMI.Config
import Kernel.External.Maps.MMI.DistanceMatrix as MMI
import Kernel.External.Maps.MMI.Geocode as MMI
import Kernel.External.Maps.MMI.MMIAuthToken as MMIAuthToken
import qualified Kernel.External.Maps.MMI.PlaceDetails as MMI
import Kernel.External.Maps.MMI.ReverseGeocoding as MMI
import Kernel.External.Maps.MMI.Routes as MMI
import Kernel.External.Maps.MMI.SnapToRoad as MMI
import qualified Kernel.External.Maps.MMI.Types as MMI
import qualified Kernel.External.Maps.MMI.Types as MMITypes
import Kernel.External.Maps.Types
import Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.CalculateDistance (getRouteLinearLength)
import Kernel.Utils.Common hiding (id)

autoSuggest ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MMICfg ->
  IT.AutoCompleteReq ->
  m IT.AutoCompleteResp
autoSuggest entityId mmiCfg req@AutoCompleteReq {..} = do
  let query = input
      loc = location
      region =
        case mmiCfg.mmiAutocompleteParams of
          Just params -> params
          Nothing ->
            case country of
              India -> "ind"
              France -> "fr"
              USA -> "us"
              Netherlands -> "nl"
      lang = language
      mapsUrl = mmiCfg.mmiNonKeyUrl
  token <- MMIAuthToken.getTokenText entityId mmiCfg
  res <- MMI.mmiAutoSuggest entityId req mapsUrl (Just $ MMITypes.MMIAuthToken token) query loc region lang
  let predictions = map (\MMITypes.SuggestedLocations {..} -> Prediction {placeId = Just eLoc, description = placeName <> " " <> placeAddress, distance = Nothing, distanceWithUnit = Nothing, types = Nothing}) res.suggestedLocations
  return $ AutoCompleteResp predictions

getDistanceMatrix ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasCoordinates a,
    HasCoordinates b,
    ToJSON a,
    ToJSON b,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MMICfg ->
  IT.GetDistancesReq a b ->
  m (NonEmpty (IT.GetDistanceResp a b))
getDistanceMatrix entityId mmiCfg req@GetDistancesReq {..} = do
  key <- decrypt mmiCfg.mmiApiKey
  let limitedOriginObjectsList = splitListByAPICap origins
      limitedDestinationObjectsList = splitListByAPICap destinations
      mapsUrl = mmiCfg.mmiKeyUrl
  res <- concatForM limitedOriginObjectsList $ \limitedOriginObjects ->
    concatForM limitedDestinationObjectsList $ \limitedDestinationObjects -> do
      let limitedOriginPlaces = map getCoordinates limitedOriginObjects
          limitedDestinationPlaces = map getCoordinates limitedDestinationObjects
          lOrigin = length limitedOriginObjects
          lDest = length limitedDestinationObjects
          strOrig = map show [0 .. lOrigin - 1]
          strDest = map show [lOrigin .. (lOrigin + lDest - 1)]
          origParam = T.intercalate ";" strOrig
          origDest = T.intercalate ";" strDest
          placesList = (++) limitedOriginPlaces limitedDestinationPlaces
          coordinatesList = map latLongToText placesList
          coordinates = T.intercalate ";" coordinatesList
      MMI.mmiDistanceMatrix entityId req mapsUrl key coordinates (Just origParam) (Just origDest)
        >>= parseDistanceMatrixResp distanceUnit lOrigin lDest limitedOriginObjects limitedDestinationObjects
  case res of
    [] -> throwError (InternalError "Empty MMI.getDistances result.")
    (a : xs) -> return $ a :| xs
  where
    splitListByAPICap inputList = do
      List.chunksOf 50 $ toList inputList

latLongToText :: LatLong -> Text
latLongToText LatLong {..} = show lon <> "," <> show lat

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

-- orig, dest, distance, duration, status
parseDistanceMatrixResp ::
  (MonadThrow m, MonadIO m, Log m) =>
  DistanceUnit ->
  Int ->
  Int ->
  [a] ->
  [b] ->
  MMITypes.DistanceMatrixResp ->
  m [IT.GetDistanceResp a b]
parseDistanceMatrixResp distanceUnit lOrigin lDest listSrc listDest distanceMatrixResp = do
  let lst = cartProd [0 .. (lOrigin - 1)] [0 .. (lDest - 1)]
  return $ map (buildResp distanceUnit listSrc listDest distanceMatrixResp) lst

buildResp ::
  DistanceUnit ->
  [a] ->
  [b] ->
  MMITypes.DistanceMatrixResp ->
  (Int, Int) ->
  IT.GetDistanceResp a b
buildResp distanceUnit listSrc listDest distanceMatrixResp pair = do
  let distance = floor $ (distanceMatrixResp.results.distances !! fst pair) !! snd pair
  GetDistanceResp
    { origin = listSrc !! fst pair,
      destination = listDest !! snd pair,
      distance,
      distanceWithUnit = convertMetersToDistance distanceUnit distance,
      duration = floor $ (distanceMatrixResp.results.durations !! fst pair) !! snd pair,
      status = distanceMatrixResp.results.code
    }

getRoutes ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MMICfg ->
  IT.GetRoutesReq ->
  m IT.GetRoutesResp
getRoutes entityId mmiCfg req = do
  key <- decrypt mmiCfg.mmiApiKey
  let origin = latLongToText (NE.head req.waypoints)
      destination = latLongToText (NE.last req.waypoints)
      points = origin <> ";" <> destination
      mapsUrl = mmiCfg.mmiKeyUrl
  resp <- MMI.mmiRoute entityId req mapsUrl key points
  traverse (mkRoute req resp) resp.routes

getPlaceDetails ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MMICfg ->
  IT.GetPlaceDetailsReq ->
  m IT.GetPlaceDetailsResp
getPlaceDetails entityId mmiCfg req@GetPlaceDetailsReq {..} = do
  key <- decrypt mmiCfg.mmiApiKey
  resp <- MMI.mmiPlaceDetails entityId req mmiCfg.mmiKeyUrl key placeId
  let MMITypes.PlaceDetail {..} = NE.head resp.results
  pure $ GetPlaceDetailsResp (LatLong {lat = latitude, lon = longitude})

mkRoute ::
  (MonadFlow m) =>
  IT.GetRoutesReq ->
  MMI.RouteResponse ->
  MMI.Routes ->
  m IT.RouteInfo
mkRoute req resp route = do
  let bound = Nothing
  if null route.legs
    then do
      logTagWarning "MMIRoutes" ("Empty route.legs, " <> show req)
      return $ RouteInfo Nothing Nothing Nothing Nothing bound [] []
    else do
      when (length route.legs > 1) $
        logTagWarning "MMIRoutes" ("More than one element in route.legs, " <> show req)
      let points = PP.decode route.geometry
          bounds = boundingBoxCal points
          boundBox = Just $ BoundingBoxWithoutCRSXY (PointXY bounds.minLat bounds.minLon) (PointXY bounds.minLat bounds.minLon)
          snappedWayPoints = (\waypoint -> waypoint.location.getLatLong) <$> resp.waypoints
          distanceInM = Just $ Meters $ double2Int route.distance
          distanceWithUnit = Just $ Distance (toHighPrecDistance route.distance) Meter
          durationInS = Just $ Seconds $ double2Int route.duration
      return $ RouteInfo durationInS Nothing distanceInM distanceWithUnit boundBox snappedWayPoints points
  where
    createAcc = Acc {minLat = 91.0, maxLat = -91.0, minLon = 180.0, maxLon = -180.0}
    boundingBoxCal points = foldl' compareLatLong createAcc points
    compareLatLong :: Acc -> LatLong -> Acc
    compareLatLong acc loc =
      let max_lat = max loc.lat acc.maxLat
          max_lng = max loc.lon acc.maxLon
          min_lat = min loc.lat acc.minLat
          min_lng = min loc.lon acc.minLon
       in Acc {minLat = min_lat, maxLat = max_lat, minLon = min_lng, maxLon = max_lng}

data Acc = Acc {minLat :: Double, maxLat :: Double, minLon :: Double, maxLon :: Double}
  deriving (Generic, ToJSON, FromJSON)

snapToRoad ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MMICfg ->
  IT.SnapToRoadReq ->
  m IT.SnapToRoadResp
snapToRoad entityId mmiCfg req = do
  key <- decrypt mmiCfg.mmiApiKey
  let points = T.intercalate ";" $ latLongToMmiText <$> req.points
      mapsUrl = mmiCfg.mmiKeyUrl
  resp <- MMI.mmiSnapToRoad entityId req mapsUrl key points

  let listOfSnappedPoints = sortOn (.waypoint_index) $ catMaybes $ resp.results.snappedPoints
  let listOfPoints = getPoints listOfSnappedPoints
  let dist = getRouteLinearLength listOfPoints req.calculateDistanceFrom
  pure
    SnapToRoadResp
      { distance = dist,
        distanceWithUnit = convertHighPrecMetersToDistance req.distanceUnit dist,
        confidence = 1.0, -- Considering MMI's default confidence as 1.0
        snappedPoints = listOfPoints
      }
  where
    getPoints :: [MMITypes.SnappedPoint] -> [LatLong]
    getPoints = fmap (\x -> x.location.getLatLong)
    latLongToMmiText :: LatLong -> Text
    latLongToMmiText LatLong {..} = show lon <> "," <> show lat

reverseGeocode ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MMICfg ->
  MMITypes.ReverseGeocodeReq ->
  m MMITypes.ReverseGeocodeResp
reverseGeocode entityId mmiCfg req@MMITypes.ReverseGeocodeReq {..} = do
  key <- decrypt mmiCfg.mmiApiKey
  let mapsUrl = mmiCfg.mmiKeyUrl
  MMI.mmiReverseGeocode entityId req mapsUrl key location region lang

geocode ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MMICfg ->
  IT.GetPlaceNameReq ->
  m IT.GetPlaceNameResp
geocode entityId mmiCfg req@GetPlaceNameReq {..} = do
  let mapsUrl = mmiCfg.mmiNonKeyUrl
  token <- MMIAuthToken.getTokenText entityId mmiCfg
  res <- MMI.mmiGeoCode entityId req mapsUrl (Just $ MMI.MMIAuthToken token) mbByPlaceId
  return [reformatePlaceName res.copResults]
  where
    reformatePlaceName (res :: MMI.GeocodeResult) =
      PlaceName
        { formattedAddress = Just res.formattedAddress,
          addressComponents = [reformateAddressResp res],
          plusCode = Just res.eLoc,
          location = LatLong {lat = 0.00, lon = 0.00},
          placeId = Nothing,
          source = Nothing
        }
    reformateAddressResp aResp =
      AddressResp
        { longName = aResp.district,
          shortName = aResp.locality,
          types = [aResp.geocodeLevel]
        }
    mbByPlaceId = case getBy of
      ByPlaceId placeId -> placeId -- Do we need to add a ByAddress?
      ByLatLong _ -> "BAD_REQUEST" --TO be fixed post discussion with MMI to add lat-long in response as per our product requirement
