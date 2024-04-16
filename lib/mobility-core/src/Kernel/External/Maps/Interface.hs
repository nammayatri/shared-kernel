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
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Utils.CalculateDistance
import Kernel.Utils.Common hiding (id)

getDistance ::
  ( EncFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  MapsServiceConfig ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistance serviceConfig GetDistanceReq {..} =
  getDistances serviceConfig getDistancesReq >>= \case
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
    HasCoordinates b
  ) =>
  MapsServiceConfig ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getDistances cfg req
  OSRMConfig cfg -> OSRM.getDistances cfg req
  MMIConfig cfg -> MMI.getDistanceMatrix cfg req
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
    Log m
  ) =>
  Bool ->
  MapsServiceConfig ->
  GetRoutesReq ->
  m GetRoutesResp
getRoutes isAvoidToll serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getRoutes isAvoidToll cfg req
  OSRMConfig osrmCfg -> OSRM.getRoutes osrmCfg req
  MMIConfig cfg -> MMI.getRoutes cfg req
  NextBillionConfig cfg -> NextBillion.getRoutes cfg req

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
    HasFlowEnv m r '["osrmMatchThreshold" ::: HighPrecMeters]
  ) =>
  Maybe MapsServiceConfig ->
  SnapToRoadHandler m ->
  SnapToRoadReq ->
  m ([MapsService], Either String SnapToRoadResp)
snapToRoadWithFallback mbMapServiceToRectifyDistantPointsFailure SnapToRoadHandler {..} req = do
  prividersList <- getProvidersList
  when (null prividersList) $ throwError $ InternalError "No maps serive provider configured"
  callSnapToRoadWithFallback prividersList
  where
    callSnapToRoadWithFallback [] = do
      logError $ "Snap to road failed with all the configured providers"
      return ([], Left "Snap to road failed with all the configured providers")
    callSnapToRoadWithFallback (preferredProvider : restProviders) = do
      mapsConfig <- getProviderConfig preferredProvider
      preCheckPassed <- runPreCheck preferredProvider req
      case (preCheckPassed, mbMapServiceToRectifyDistantPointsFailure) of
        (False, Nothing) -> do
          logError $ "Pre check failed for provider " <> show preferredProvider
          callSnapToRoadWithFallback restProviders
        (False, Just mapServiceCfg) -> do
          droppedPointsThreshold <- asks (.droppedPointsThreshold)
          maxStraightLineRectificationThreshold <- asks (.maxStraightLineRectificationThreshold)
          let starightDistancePoints = getEverySnippetWhichIsNot (< droppedPointsThreshold) req.points
          distanceRectified <-
            mapM
              ( \(x1, x2, dist) -> do
                  if dist < maxStraightLineRectificationThreshold
                    then pure (x1, dist)
                    else do
                      distanceRes <- getDistance mapServiceCfg (GetDistanceReq {origin = x1, destination = x2, travelMode = Just CAR} :: GetDistanceReq LatLong LatLong)
                      pure (x1, metersToHighPrecMeters distanceRes.distance)
              )
              starightDistancePoints
          let (pointsOutOfThreshold, distance) = foldl' (\(accPoints, accDis) (x1, dis) -> (accPoints <> [x1], accDis + dis)) ([], 0) distanceRectified
          let splitSnapToRoadCalls = filter (not . (<= 1) . length) $ splitWith pointsOutOfThreshold req.points
          pointsRes <- try @_ @SomeException $ mapM (\section -> snapToRoad mapsConfig (req {points = section})) splitSnapToRoadCalls
          case pointsRes of
            Left _ -> do
              (servicesUsed, snapResponse) <- callSnapToRoadWithFallback restProviders
              return (preferredProvider : servicesUsed, snapResponse)
            Right results -> do
              let (totalSectorsDistance, snappedPoints) = foldl' (\(accDis, snappedPoints') res -> (res.distance + accDis, snappedPoints' <> res.snappedPoints)) (0, []) results
                  snappedResp =
                    SnapToRoadResp
                      { distance = totalSectorsDistance + distance,
                        confidence = 1,
                        snappedPoints = snappedPoints
                      }
              return ([preferredProvider, SelfTuned], Right snappedResp)
        (True, _) -> do
          result <- try @_ @SomeException $ snapToRoad mapsConfig req
          case result of
            Left _ -> do
              (servicesUsed, snapResponse) <- callSnapToRoadWithFallback restProviders
              return (preferredProvider : servicesUsed, snapResponse)
            Right res -> do
              confidencethreshold <- getConfidenceThreshold
              postCheckPassed <- runPostCheck preferredProvider req res
              when (not postCheckPassed) $ logError $ "Post check failed for provider " <> show preferredProvider
              if res.confidence < confidencethreshold || not postCheckPassed
                then do
                  (servicesUsed, snapResponse) <- callSnapToRoadWithFallback restProviders
                  return (preferredProvider : servicesUsed, snapResponse)
                else return ([preferredProvider], Right res)

snapToRoad ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  MapsServiceConfig ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad serviceConfig req =
  case serviceConfig of
    GoogleConfig cfg -> Google.snapToRoad cfg req
    OSRMConfig osrmCfg -> OSRM.callOsrmMatch osrmCfg req
    MMIConfig mmiCfg -> MMI.snapToRoad mmiCfg req
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
    CoreMetrics m
  ) =>
  MapsServiceConfig ->
  AutoCompleteReq ->
  m AutoCompleteResp
autoComplete serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.autoComplete cfg req
  OSRMConfig _ -> throwNotProvidedError "autoComplete" OSRM
  MMIConfig cfg -> MMI.autoSuggest cfg req
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
    CoreMetrics m
  ) =>
  MapsServiceConfig ->
  GetPlaceDetailsReq ->
  m GetPlaceDetailsResp
getPlaceDetails serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getPlaceDetails cfg req
  OSRMConfig _ -> throwNotProvidedError "getPlaceDetails" OSRM
  MMIConfig cfg -> MMI.getPlaceDetails cfg req
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
    Redis.HedisFlow m r
  ) =>
  MapsServiceConfig ->
  GetPlaceNameReq ->
  m GetPlaceNameResp
getPlaceName serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getPlaceName cfg req
  OSRMConfig _ -> throwNotProvidedError "getPlaceName" OSRM
  MMIConfig cfg -> MMI.geocode cfg req
  NextBillionConfig _ -> throwNotProvidedError "getPlaceName" NextBillion
