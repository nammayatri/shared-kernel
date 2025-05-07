{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.Interface.NextBillion
  ( module Reexport,
    getRoutes,
    getRoutesWithExtraParameters,
  )
where

import qualified Data.List.NonEmpty as NE
import GHC.Float (double2Int)
import Kernel.External.Encryption
import Kernel.External.Maps.Google.Config as Reexport
import qualified Kernel.External.Maps.Google.MapsClient as GoogleMaps
import qualified Kernel.External.Maps.Google.PolyLinePoints as PP
import Kernel.External.Maps.HasCoordinates as Reexport (HasCoordinates (..))
import Kernel.External.Maps.Interface.Types
import Kernel.External.Maps.NextBillion.Config
import Kernel.External.Maps.NextBillion.Route as NB
import Kernel.External.Maps.NextBillion.Types as NextBillion
import Kernel.External.Maps.Types as Reexport
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

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

convertToRoute :: NextBillion.Route -> RouteInfo
convertToRoute route =
  RouteInfo
    { duration = Just (Seconds $ double2Int route.duration),
      distance = Just (Meters $ double2Int route.distance),
      staticDuration = Nothing,
      distanceWithUnit = Just $ Distance (toHighPrecDistance route.distance) Meter,
      boundingBox = Nothing,
      snappedWaypoints = [],
      points = PP.decode $ route.geometry
    }

latLongToPlace :: LatLong -> GoogleMaps.Place
latLongToPlace LatLong {..} =
  GoogleMaps.Location $ GoogleMaps.LocationS {lat = lat, lng = lon}

getWayPoints :: [LatLong] -> Maybe [GoogleMaps.Place]
getWayPoints [] = Nothing
getWayPoints waypoints = Just (map latLongToPlace waypoints)

getRoutes ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  NextBillionCfg ->
  GetRoutesReq ->
  m GetRoutesResp
getRoutes entityId cfg req = do
  let routeProxyReq = routeToRouteProxyConverter req
  let url = cfg.nextBillionDirectionsUrl
  let origin = latLongToPlace routeProxyReq.origin
      destination = latLongToPlace routeProxyReq.destination
      waypoints = getWayPoints routeProxyReq.waypoints
  key <- decrypt cfg.nextBillionKey
  res <- NB.directions entityId req url key origin destination waypoints Nothing Nothing Nothing Nothing
  let allRoutes = map convertToRoute res.routes
  return $ allRoutes

getRoutesWithExtraParameters ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  NextBillionCfg ->
  NextBillion.GetRoutesRequest ->
  m GetRoutesResp
getRoutesWithExtraParameters entityId cfg req = do
  let url = cfg.nextBillionDirectionsUrl
      origin = latLongToPlace $ NE.head req.waypoints
      destination = latLongToPlace $ NE.last req.waypoints
      waypoints = getWayPoints $ originAndDestinationRemover $ NE.toList req.waypoints
  key <- decrypt cfg.nextBillionKey
  res <- NB.directions entityId req url key origin destination waypoints req.alternatives req.altcount req.routeType req.option
  let allRoutes = map convertToRoute res.routes
  return $ allRoutes
