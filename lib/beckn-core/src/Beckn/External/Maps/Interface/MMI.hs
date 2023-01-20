module Beckn.External.Maps.Interface.MMI
  ( autoSuggest,
    getRoutes,
  )
where

import Beckn.External.Encryption
import Beckn.External.Maps.Google.PolyLinePoints as PP
import Beckn.External.Maps.Interface.Types as IT
import Beckn.External.Maps.MMI.AutoSuggest as MMI
import Beckn.External.Maps.MMI.Config
import Beckn.External.Maps.MMI.MMIAuthToken as MMIAuthToken
import qualified Beckn.External.Maps.MMI.MapsClient.Types as MMI
import Beckn.External.Maps.MMI.Routes as MMI
import qualified Beckn.External.Maps.Types as MapT
import Beckn.Storage.Hedis as Redis
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Utils.Common
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import EulerHS.Prelude
import GHC.Float (double2Int)

autoSuggest ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  MMICfg ->
  IT.AutoCompleteReq ->
  m IT.AutoCompleteResp
autoSuggest mmiCfg AutoCompleteReq {..} = do
  let query = input
      loc = location
      region = "ind"
      lang = language
      mapsUrl = mmiCfg.mmiNonKeyUrl
  token <- MMIAuthToken.getTokenText mmiCfg
  res <- MMI.mmiAutoSuggest mapsUrl (Just $ MMI.MMIAuthToken token) query loc region lang
  let predictions = map (\MMI.SuggestedLocations {..} -> Prediction {placeId = Just eLoc, description = placeName <> " " <> placeAddress}) res.suggestedLocations
  return $ AutoCompleteResp predictions

getRoutes ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m
  ) =>
  MMICfg ->
  IT.GetRoutesReq ->
  m IT.GetRoutesResp
getRoutes mmiCfg req = do
  key <- decrypt mmiCfg.mmiApiKey
  let origin = latLongToText (NE.head req.waypoints)
      destination = latLongToText (NE.last req.waypoints)
      points = origin <> ";" <> destination
      mapsUrl = mmiCfg.mmiKeyUrl
  resp <- MMI.mmiRoute mapsUrl key points
  traverse (mkRoute req resp) resp.routes

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
      return $ RouteInfo Nothing Nothing bound [] []
    else do
      when (length route.legs > 1) $
        logTagWarning "MMIRoutes" ("More than one element in route.legs, " <> show req)
      let points = PP.decode route.geometry
          bounds = boundingBoxCal points
          boundBox = Just $ BoundingBoxWithoutCRSXY (PointXY bounds.minLat bounds.minLon) (PointXY bounds.minLat bounds.minLon)
          snappedWayPoints = (\waypoint -> (waypoint.location.getLatLong)) <$> resp.waypoints
          distanceInM = Just $ Meters $ double2Int route.distance
          durationInS = Just $ Seconds $ double2Int route.duration
      return $ RouteInfo durationInS distanceInM boundBox snappedWayPoints points
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

latLongToText :: MapT.LatLong -> Text
latLongToText MapT.LatLong {..} = show lon <> "," <> show lat
