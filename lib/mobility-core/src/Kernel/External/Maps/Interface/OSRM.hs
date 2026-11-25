{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=x-partial #-}

module Kernel.External.Maps.Interface.OSRM
  ( module Reexport,
    callOsrmMatch,
    getDistances,
    getNearestPoints,
    getOSRMTable,
    getRoutes,
    classifySpeed,
    legTiers,
    mkTrafficSegments,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
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
  Maybe Text ->
  OSRMCfg ->
  SnapToRoadReq ->
  m SnapToRoadResp
callOsrmMatch entityId merchantCityId osrmCfg req@(SnapToRoadReq wps distanceUnit calculateDistanceFrom) = do
  let mbRadius = fmap (.getMeters) osrmCfg.radiusDeviation
  res <- OSRM.callOsrmMatchAPI entityId merchantCityId req osrmCfg.osrmUrl mbRadius CAR (OSRM.PointsList wps)
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
  Maybe Text ->
  OSRMCfg ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances entityId merchantCityId osrmCfg request = do
  let pointsList = OSRM.PointsList $ map getCoordinates (toList request.origins) ++ map getCoordinates (toList request.destinations)
  let sourcesList = OSRM.SourcesList [0 .. (length request.origins - 1)]
  let destinationsList = OSRM.DestinationsList [(length request.origins) .. (length request.origins + length request.destinations - 1)]
  response <- OSRM.callOsrmGetDistancesAPI entityId merchantCityId request osrmCfg.osrmUrl (fromMaybe CAR request.travelMode) pointsList sourcesList destinationsList request.sourceDestinationMapping
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
  Maybe Text ->
  OSRMCfg ->
  GetRoutesReq ->
  m GetRoutesResp
getRoutes entityId merchantCityId osrmCfg request = do
  let mbTrafficCfg = osrmCfg.trafficConfig
  response <-
    OSRM.callOsrmRouteAPI entityId merchantCityId request osrmCfg.osrmUrl (fromMaybe CAR request.mode) (OSRM.PointsList {getPointsList = NE.toList request.waypoints}) (isJust mbTrafficCfg)
  getOSRMRoute mbTrafficCfg response

convertRouteToRouteInfo :: (Log m, MonadThrow m) => Maybe OSRMTrafficConfig -> OSRM.OSRMRouteRoutes -> m RouteInfo
convertRouteToRouteInfo mbTrafficCfg osrmRouteRoutes =
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
            routeToken = Nothing,
            trafficSegments = mkTrafficSegments mbTrafficCfg osrmRouteRoutes.legs
          }

data ClassifiedSegment = ClassifiedSegment
  { index :: Int,
    speedClass :: TrafficSpeed,
    lengthMeters :: Double
  }

-- | Per-segment inputs of one leg, each list as long as the leg's @annotation.speed@.
data LegMeasurements = LegMeasurements
  { measuredSpeeds :: [Double],
    measuredDistances :: [Double],
    measuredTiers :: [Maybe RoadTier]
  }

mkTrafficSegments :: Maybe OSRMTrafficConfig -> [OSRM.RouteResponseLeg] -> Maybe [RouteTrafficSegment]
mkTrafficSegments Nothing _ = Nothing
mkTrafficSegments (Just cfg) legs
  | null measuredLegs = Nothing
  | otherwise = Just . concat $ zipWith (legSegments cfg) offsets measuredLegs
  where
    measuredLegs = catMaybes . takeWhile isJust $ map legMeasurements legs -- fix this later to take later legs as well
    offsets = scanl (+) 0 $ map (length . (.measuredSpeeds)) measuredLegs -- scanl (+) prefix sum h
    legMeasurements leg = do
      annotation <- leg.annotation
      speeds <- annotation.speed
      let tiers = legTiers leg
      pure
        LegMeasurements
          { measuredSpeeds = speeds,
            measuredDistances = fromMaybe [] annotation.distance,
            measuredTiers = if length tiers == length speeds then tiers else []
          }

-- | The road tier of every segment of a leg, in @annotation.speed@ order.
--
-- OSRM reports road classes per junction, under @steps[].intersections[]@, while speeds come
-- per segment, a junction's tier holds until the next one.
legTiers :: OSRM.RouteResponseLeg -> [Maybe RoadTier]
legTiers leg = concatMap stepTiers (dropLast leg.steps)
  where
    dropLast [] = []
    dropLast steps = init steps

    stepTiers step =
      let coordinates = map (.getLatLong) step.geometry.coordinates
          segmentCount = length coordinates - 1
       in spread segmentCount $ getIntersectionIndex (zip [0 ..] coordinates) (fromMaybe [] step.intersections)

    -- will output something like [(3,Main),(10, Arterial)...]
    getIntersectionIndex _ [] = []
    getIntersectionIndex indexed (intersection : rest) =
      case dropWhile (\(_, point) -> intersection.location.getLatLong /= point) indexed of
        [] -> getIntersectionIndex indexed rest
        found@((foundIndex, _) : _) -> (foundIndex, roadTier intersection) : getIntersectionIndex found rest

    -- A junction's tier covers the segments from it up to the next junction.
    spread segmentCount [] = replicate segmentCount Nothing
    spread segmentCount marks@((firstIndex, _) : _) =
      replicate (clamp segmentCount firstIndex) Nothing
        <> concat (zipWith (run segmentCount) marks (map fst (drop 1 marks) <> [segmentCount]))

    run segmentCount (start, tier) end = replicate (clamp segmentCount end - clamp segmentCount start) tier

    clamp x boundary = max 0 (min x boundary)

    roadTier intersection = (listToMaybe . mapMaybe roadTierFromClass) =<< intersection.classes

legSegments :: OSRMTrafficConfig -> Int -> LegMeasurements -> [RouteTrafficSegment]
legSegments cfg offset measurements = mapMaybe toSegment $ runs classified
  where
    classified =
      zipWith3
        (\index (speed, tier) lengthMeters -> ClassifiedSegment {index, speedClass = classifySpeed cfg tier speed, lengthMeters})
        [offset ..]
        (zip measurements.measuredSpeeds (measurements.measuredTiers <> repeat Nothing))
        (measurements.measuredDistances <> repeat 0)

    runs [] = []
    runs (x : xs) = let (same, rest) = span (\y -> y.speedClass == x.speedClass) xs in (x : same) : runs rest

    toSegment run = do
      firstSegment <- listToMaybe run
      lastSegment <- listToMaybe $ reverse run
      guard $ firstSegment.speedClass /= TrafficNormal
      guard $ sum (map (.lengthMeters) run) >= cfg.minSegmentLengthMeters
      pure RouteTrafficSegment {startIndex = firstSegment.index, endIndex = lastSegment.index + 1, speed = firstSegment.speedClass}

classifySpeed :: OSRMTrafficConfig -> Maybe RoadTier -> Double -> TrafficSpeed
classifySpeed cfg mbTier speedInMetersPerSecond
  | speedInKmph <= jamBelow = TrafficJam
  | speedInKmph <= slowBelow = TrafficSlow
  | otherwise = TrafficNormal
  where
    speedInKmph = speedInMetersPerSecond * 3.6
    (jamBelow, slowBelow) = case mbTierThresholds of
      Just thresholds -> (thresholds.jamBelowKmph, thresholds.slowBelowKmph)
      Nothing -> (cfg.jamBelowKmph, cfg.slowBelowKmph)
    mbTierThresholds = do
      tier <- mbTier
      perTier <- cfg.tierThresholds
      M.lookup tier perTier

getOSRMRoute ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  Maybe OSRMTrafficConfig ->
  OSRM.OSRMRouteResponse ->
  m GetRoutesResp
getOSRMRoute mbTrafficCfg osrmRouteResponse = sequence $ map (convertRouteToRouteInfo mbTrafficCfg) osrmRouteResponse.routes

getNearestPoints ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r,
    HasRequestId r
  ) =>
  Maybe Text ->
  Maybe Text ->
  OSRMCfg ->
  GetNearestPointsReq ->
  m GetNearestPointsResp
getNearestPoints entityId merchantCityId osrmCfg req = do
  response <- OSRM.callOsrmNearestAPI entityId merchantCityId req osrmCfg.osrmUrl CAR (OSRM.PointsList [req.point]) req.number
  pure . GetNearestPointsResp $ map mkNearestPoint response.waypoints
  where
    mkNearestPoint waypoint =
      NearestPoint
        { location = waypoint.location.getLatLong,
          roadName = waypoint.name,
          distance = waypoint.distance
        }
