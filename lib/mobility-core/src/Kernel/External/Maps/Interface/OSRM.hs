{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.Interface.OSRM
  ( module Reexport,
    callOsrmMatch,
    getDistances,
    getOSRMTable,
    getRoutes,
  )
where

import qualified Data.List.NonEmpty as NE
import GHC.Float (double2Int)
import Kernel.External.Maps.Google.Config as Reexport
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.External.Maps.HasCoordinates as Reexport (HasCoordinates (..))
import Kernel.External.Maps.Interface.Types
import Kernel.External.Maps.OSRM.Config
import Kernel.External.Maps.OSRM.RoadsClient
import qualified Kernel.External.Maps.OSRM.RoadsClient as OSRM
import Kernel.External.Maps.Types as Reexport
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.CalculateDistance (getRouteLinearLength)
import Kernel.Utils.Common

callOsrmMatch ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r,
    HasRequestId r
  ) =>
  Maybe Text ->
  OSRMCfg ->
  SnapToRoadReq ->
  m SnapToRoadResp
callOsrmMatch entityId osrmCfg req@(SnapToRoadReq wps distanceUnit calculateDistanceFrom) = do
  let mbRadius = fmap (.getMeters) osrmCfg.radiusDeviation
  res <- OSRM.callOsrmMatchAPI entityId req osrmCfg.osrmUrl mbRadius CAR (OSRM.PointsList wps)
  (dist, conf, interpolatedPts) <- OSRM.getResultOneRouteExpected res
  pure $ case calculateDistanceFrom of
    Just _ -> do
      let dist' = getRouteLinearLength interpolatedPts calculateDistanceFrom
      SnapToRoadResp dist' (convertHighPrecMetersToDistance distanceUnit dist') conf interpolatedPts
    Nothing -> SnapToRoadResp dist (convertHighPrecMetersToDistance distanceUnit dist) conf interpolatedPts

getDistances ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    HasCoordinates a,
    HasCoordinates b,
    ToJSON a,
    ToJSON b,
    MonadReader r m,
    HasKafkaProducer r,
    HasRequestId r
  ) =>
  Maybe Text ->
  OSRMCfg ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances entityId osrmCfg request = do
  let pointsList = OSRM.PointsList $ map getCoordinates (toList request.origins) ++ map getCoordinates (toList request.destinations)
  let sourcesList = OSRM.SourcesList [0 .. (length request.origins - 1)]
  let destinationsList = OSRM.DestinationsList [(length request.origins) .. (length request.origins + length request.destinations - 1)]
  response <- OSRM.callOsrmGetDistancesAPI entityId request osrmCfg.osrmUrl (fromMaybe CAR request.travelMode) pointsList sourcesList destinationsList request.sourceDestinationMapping
  case request.sourceDestinationMapping of
    Just OneToOne -> getOSRMTableOneToOne response request
    _ -> getOSRMTable response request

getOSRMTable ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  OSRM.OSRMTableResponse ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getOSRMTable tableResponse request = do
  let pairOfIndexSource = zip [0 .. (length request.origins - 1)] (NE.toList request.origins)
  let pairOfIndexDestinations = zip [0 .. (length request.destinations - 1)] (NE.toList $ request.destinations)

  pure $
    NE.fromList $
      foldl
        ( \mainList sourcePair ->
            do
              let subList =
                    map
                      ( \destinationPair -> do
                          let rawDistance = Meters {getMeters = round $ tableResponse.distances !! fst sourcePair !! fst destinationPair}
                          let rawDuration = Seconds {getSeconds = round $ tableResponse.durations !! fst sourcePair !! fst destinationPair}
                          let distance = if rawDistance < -1 then Meters {getMeters = 2000} else rawDistance
                          let duration = if rawDuration < -1 then Seconds {getSeconds = 2000} else rawDuration
                          GetDistanceResp
                            { origin = snd sourcePair,
                              destination = snd destinationPair,
                              distance,
                              distanceWithUnit = convertMetersToDistance request.distanceUnit distance,
                              duration = duration,
                              status = "ok"
                            }
                      )
                      pairOfIndexDestinations
              mainList ++ subList
        )
        []
        pairOfIndexSource

getOSRMTableOneToOne ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  OSRM.OSRMTableResponse ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getOSRMTableOneToOne tableResponse request = do
  let pairOfIndexSourceDest = zip3 [0 .. (length request.origins - 1)] (NE.toList request.origins) (NE.toList $ request.destinations)
  pure $
    NE.fromList $
      foldl
        ( \mainList (indx, source, dest) -> do
            let rawDistance = Meters {getMeters = round $ tableResponse.distances !! 0 !! indx}
            let rawDuration = Seconds {getSeconds = round $ tableResponse.durations !! 0 !! indx}
            let distance = if rawDistance < -1 then Meters {getMeters = 2000} else rawDistance
            let duration = if rawDuration < -1 then Seconds {getSeconds = 2000} else rawDuration
            let resp =
                  GetDistanceResp
                    { origin = source,
                      destination = dest,
                      distance,
                      distanceWithUnit = convertMetersToDistance request.distanceUnit distance,
                      duration = duration,
                      status = "ok"
                    }
            let subList = [resp]
            mainList ++ subList
        )
        []
        pairOfIndexSourceDest

getRoutes ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r,
    HasRequestId r
  ) =>
  Maybe Text ->
  OSRMCfg ->
  GetRoutesReq ->
  m GetRoutesResp
getRoutes entityId osrmCfg request = do
  response <- OSRM.callOsrmRouteAPI entityId request osrmCfg.osrmUrl (fromMaybe CAR request.mode) $ OSRM.PointsList {getPointsList = NE.toList request.waypoints}
  getOSRMRoute response

convertRouteToRouteInfo :: (Log m, MonadThrow m) => OSRM.OSRMRouteRoutes -> m RouteInfo
convertRouteToRouteInfo osrmRouteRoutes =
  if length osrmRouteRoutes.legs < 1
    then do
      throwError $ InternalError "OSRM snapped waypoints has no routes"
    else
      return
        RouteInfo
          { distance = Just $ Meters $ double2Int $ osrmRouteRoutes.distance,
            distanceWithUnit = Just $ Distance (toHighPrecDistance osrmRouteRoutes.distance) Meter,
            duration = Just $ Seconds $ double2Int $ osrmRouteRoutes.duration,
            staticDuration = Nothing,
            points = map (.getLatLong) osrmRouteRoutes.geometry.coordinates,
            snappedWaypoints = map (\steps -> getLatLong $ head steps.geometry.coordinates) $ OSRM.steps $ head osrmRouteRoutes.legs,
            boundingBox = Nothing,
            isDefaultRoute = False
          }

getOSRMRoute ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  OSRM.OSRMRouteResponse ->
  m GetRoutesResp
getOSRMRoute osrmRouteResponse = sequence $ map convertRouteToRouteInfo osrmRouteResponse.routes
