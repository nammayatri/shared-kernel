{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.Interface
  ( module Reexport,
    getDistance,
    getDistancesProvided,
    getDistances,
    getRoutesProvided,
    getRoutes,
    snapToRoadProvided,
    snapToRoad,
    snapToRoadWithFallback,
    autoCompleteProvided,
    autoComplete,
    getPlaceDetailsProvided,
    getPlaceDetails,
    getPlaceNameProvided,
    getPlaceName,
  )
where

import EulerHS.Prelude ((...))
import Kernel.External.Maps.Google.Config as Reexport
import Kernel.External.Maps.HasCoordinates as Reexport (HasCoordinates (..))
import qualified Kernel.External.Maps.Interface.Google as Google
import qualified Kernel.External.Maps.Interface.MMI as MMI
import qualified Kernel.External.Maps.Interface.NextBillion as NextBillion
import qualified Kernel.External.Maps.Interface.OSRM as OSRM
import Kernel.External.Maps.Interface.Types as Reexport
import Kernel.External.Maps.MMI.Config as Reexport
import Kernel.External.Maps.OSRM.Config as Reexport
import Kernel.External.Maps.Types as Reexport
import Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Utils.CalculateDistance
import Kernel.Utils.Common hiding (id)

getDistance ::
  ( EncFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b,
    ToJSON a,
    ToJSON b,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MapsServiceConfig ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistance entityId serviceConfig GetDistanceReq {..} =
  getDistances entityId serviceConfig getDistancesReq >>= \case
    (a :| []) -> return a
    _ -> throwError (InternalError "Exactly one getDistance result expected.")
  where
    getDistancesReq =
      GetDistancesReq
        { origins = origin :| [],
          destinations = destination :| [],
          ..
        }

mkNotProvidedError :: Text -> MapsService -> Text
mkNotProvidedError functionName serviceName = "Function " <> functionName <> " is not provided by service " <> show serviceName

throwNotProvidedError :: (MonadFlow m) => Text -> MapsService -> m a
throwNotProvidedError =
  (throwError . InternalError) ... mkNotProvidedError

getDistancesProvided :: MapsService -> Bool
getDistancesProvided = \case
  Google -> True
  OSRM -> False
  MMI -> True
  NextBillion -> False
  SelfTuned -> False

-- FIXME this logic is redundant, because we throw error always when getDistancesProvided service = False
getDistances ::
  ( EncFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b,
    ToJSON a,
    ToJSON b,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MapsServiceConfig ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances entityId serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getDistances entityId cfg req
  OSRMConfig cfg -> OSRM.getDistances entityId cfg req
  MMIConfig cfg -> MMI.getDistanceMatrix entityId cfg req
  NextBillionConfig _ -> throwNotProvidedError "getDistances" NextBillion

getRoutesProvided :: MapsService -> Bool
getRoutesProvided = \case
  Google -> True
  OSRM -> False
  MMI -> False
  NextBillion -> True
  SelfTuned -> False

getRoutes ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  Bool ->
  MapsServiceConfig ->
  GetRoutesReq ->
  m GetRoutesResp
getRoutes entityId isAvoidToll serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getRoutes entityId isAvoidToll cfg req
  OSRMConfig osrmCfg -> OSRM.getRoutes entityId osrmCfg req
  MMIConfig cfg -> MMI.getRoutes entityId cfg req
  NextBillionConfig cfg -> NextBillion.getRoutes entityId cfg req

snapToRoadProvided :: MapsService -> Bool
snapToRoadProvided = \case
  Google -> True
  OSRM -> True
  MMI -> True
  NextBillion -> False
  SelfTuned -> True

runPreCheck ::
  ( EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["droppedPointsThreshold" ::: HighPrecMeters]
  ) =>
  MapsService ->
  SnapToRoadReq ->
  m Bool
runPreCheck mapsService req = do
  droppedPointsThreshold <- asks (.droppedPointsThreshold)
  case mapsService of
    Google -> return (everySnippetIs (< droppedPointsThreshold) req.points)
    MMI -> return (everySnippetIs (< droppedPointsThreshold) req.points)
    OSRM -> return True
    _ -> return True

runPostCheck ::
  ( EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["osrmMatchThreshold" ::: HighPrecMeters]
  ) =>
  MapsService ->
  SnapToRoadReq ->
  SnapToRoadResp ->
  m Bool
runPostCheck mapsService req res = do
  snippetThreshold <- asks (.snapToRoadSnippetThreshold)
  osrmThreshold <- asks (.osrmMatchThreshold)
  case mapsService of
    Google -> return (everySnippetIs (< snippetThreshold) res.snappedPoints)
    MMI -> return (everySnippetIs (< snippetThreshold) res.snappedPoints)
    OSRM -> return $ (< osrmThreshold) $ distanceBetweenInMeters (last req.points) (last res.snappedPoints)
    _ -> return True

snapToRoadWithFallback ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["droppedPointsThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["maxStraightLineRectificationThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["osrmMatchThreshold" ::: HighPrecMeters],
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  Maybe MapsServiceConfig ->
  SnapToRoadHandler m ->
  SnapToRoadReq ->
  m ([MapsService], Either String SnapToRoadResp)
snapToRoadWithFallback entityId mbMapServiceToRectifyDistantPointsFailure SnapToRoadHandler {..} req = do
  providersList <- getProvidersList
  when (null providersList) $ throwError $ InternalError "No maps service provider configured"
  (servicesUsed, snapResponse) <- callSnapToRoadWithFallback providersList
  case (snapResponse, mbMapServiceToRectifyDistantPointsFailure) of
    (Right resp, _) -> return (servicesUsed, Right resp)
    (Left err, Nothing) -> return (servicesUsed, Left err)
    (Left _, Just mapServiceCfg) -> do
      (rectificationServicesUsed, snapToRoadResponse) <- callSnapToRoadWithRectification mapServiceCfg providersList
      return (servicesUsed ++ rectificationServicesUsed, snapToRoadResponse)
  where
    callSnapToRoadWithFallback [] = do
      logError $ "Snap to road failed with all the configured providers"
      return ([], Left "Snap to road failed with all the configured providers")
    callSnapToRoadWithFallback (preferredProvider : restProviders) = do
      mapsConfig <- getProviderConfig preferredProvider
      preCheckPassed <- runPreCheck preferredProvider req
      if preCheckPassed
        then do
          result <- try @_ @SomeException $ snapToRoad entityId mapsConfig req
          case result of
            Left err -> do
              logError $ "Snap to road Pre Check failed with error : " <> show err <> " - Provider : " <> show preferredProvider
              (servicesUsed, snapResponse) <- callSnapToRoadWithFallback restProviders
              return (servicesUsed ++ [preferredProvider], snapResponse)
            Right res -> do
              confidencethreshold <- getConfidenceThreshold
              postCheckPassed <- runPostCheck preferredProvider req res
              when (not postCheckPassed) $ logError $ "Snap to road Post Check failed - Povider : " <> show preferredProvider
              let confidenceOutOfRange = res.confidence < confidencethreshold || res.confidence > 1.0
              when (confidenceOutOfRange) $ logError $ "Snap to road Post Check failed due to Confidence : " <> show res.confidence <> " - Provider : " <> show preferredProvider
              if confidenceOutOfRange || not postCheckPassed
                then do
                  (servicesUsed, snapResponse) <- callSnapToRoadWithFallback restProviders
                  return (servicesUsed ++ [preferredProvider], snapResponse)
                else return ([preferredProvider], Right res)
        else do
          (servicesUsed, snapResponse) <- callSnapToRoadWithFallback restProviders
          return (servicesUsed ++ [preferredProvider], snapResponse)

    callSnapToRoadWithRectification _ [] = do
      logError $ "Snap to road rectification failed with all the configured providers"
      return ([], Left "Snap to road rectification failed with all the configured providers")
    callSnapToRoadWithRectification mapServiceCfg (preferredProvider : restProviders) = do
      mapsConfig <- getProviderConfig preferredProvider
      droppedPointsThreshold <- asks (.droppedPointsThreshold)
      maxStraightLineRectificationThreshold <- asks (.maxStraightLineRectificationThreshold)
      let straightDistancePoints = getEverySnippetWhichIsNot (< droppedPointsThreshold) req.points
      distanceRectified <-
        mapM
          ( \(x1, x2, dist) -> do
              if dist < maxStraightLineRectificationThreshold
                then pure (x1, dist)
                else do
                  distanceRes <- getDistance entityId mapServiceCfg (GetDistanceReq {origin = x1, destination = x2, travelMode = Just CAR, distanceUnit = req.distanceUnit, sourceDestinationMapping = Nothing} :: GetDistanceReq LatLong LatLong)
                  pure (x1, metersToHighPrecMeters distanceRes.distance)
          )
          straightDistancePoints
      logDebug $ "Rectified distances: " <> show distanceRectified
      let (pointsOutOfThreshold, distance) = foldl' (\(accPoints, accDis) (x1, dis) -> (accPoints <> [x1], accDis + dis)) ([], 0) distanceRectified
      let splitSnapToRoadCalls = filter (not . (<= 1) . length) $ splitWith pointsOutOfThreshold req.points
      logDebug $ "Split snap-to-road calls: " <> show splitSnapToRoadCalls
      pointsRes <- try @_ @SomeException $ mapM (\section -> snapToRoad entityId mapsConfig (req {points = section})) splitSnapToRoadCalls
      logDebug $ "Snap-to-road results: " <> show pointsRes
      case pointsRes of
        Right result -> do
          let (totalSectorsDistance, snappedPoints) = foldl' (\(accDis, snappedPoints') res -> (res.distance + accDis, snappedPoints' <> res.snappedPoints)) (0, []) result
          let snapToRoadResp =
                SnapToRoadResp
                  { distance = totalSectorsDistance + distance,
                    distanceWithUnit = convertHighPrecMetersToDistance req.distanceUnit $ totalSectorsDistance + distance,
                    confidence = 1,
                    snappedPoints = snappedPoints
                  }
          return ([preferredProvider, SelfTuned], Right snapToRoadResp)
        Left err -> do
          logError $ "Snap to road rectification failed with error : " <> show err <> " - Provider : " <> show preferredProvider
          (servicesUsed, snapResponse) <- callSnapToRoadWithRectification mapServiceCfg restProviders
          return (servicesUsed ++ [preferredProvider, SelfTuned], snapResponse)

snapToRoad ::
  ( EncFlow m r,
    CoreMetrics m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MapsServiceConfig ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad entityId serviceConfig req =
  case serviceConfig of
    GoogleConfig cfg -> Google.snapToRoad entityId cfg req
    OSRMConfig osrmCfg -> OSRM.callOsrmMatch entityId osrmCfg req
    MMIConfig mmiCfg -> MMI.snapToRoad entityId mmiCfg req
    NextBillionConfig _ -> throwNotProvidedError "snapToRoad" NextBillion

autoCompleteProvided :: MapsService -> Bool
autoCompleteProvided = \case
  Google -> True
  OSRM -> False
  MMI -> True
  NextBillion -> False
  SelfTuned -> False

autoComplete ::
  ( EncFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MapsServiceConfig ->
  AutoCompleteReq ->
  m AutoCompleteResp
autoComplete entityId serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.autoComplete entityId cfg req
  OSRMConfig _ -> throwNotProvidedError "autoComplete" OSRM
  MMIConfig cfg -> MMI.autoSuggest entityId cfg req
  NextBillionConfig _ -> throwNotProvidedError "autoComplete" NextBillion

getPlaceDetailsProvided :: MapsService -> Bool
getPlaceDetailsProvided = \case
  Google -> True
  OSRM -> False
  MMI -> True
  NextBillion -> False
  SelfTuned -> False

getPlaceDetails ::
  ( EncFlow m r,
    CoreMetrics m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MapsServiceConfig ->
  GetPlaceDetailsReq ->
  m GetPlaceDetailsResp
getPlaceDetails entityId serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getPlaceDetails entityId cfg req
  OSRMConfig _ -> throwNotProvidedError "getPlaceDetails" OSRM
  MMIConfig cfg -> MMI.getPlaceDetails entityId cfg req
  NextBillionConfig _ -> throwNotProvidedError "getPlaceDetails" NextBillion

getPlaceNameProvided :: MapsService -> Bool
getPlaceNameProvided = \case
  Google -> True
  OSRM -> False
  MMI -> True
  NextBillion -> False
  SelfTuned -> False

getPlaceName ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MapsServiceConfig ->
  GetPlaceNameReq ->
  m GetPlaceNameResp
getPlaceName entityId serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getPlaceName entityId cfg req
  OSRMConfig _ -> throwNotProvidedError "getPlaceName" OSRM
  MMIConfig cfg -> MMI.geocode entityId cfg req
  NextBillionConfig _ -> throwNotProvidedError "getPlaceName" NextBillion
