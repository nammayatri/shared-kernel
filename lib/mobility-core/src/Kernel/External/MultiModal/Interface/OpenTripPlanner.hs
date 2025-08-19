module Kernel.External.MultiModal.Interface.OpenTripPlanner (getTransitRoutes) where

import Data.Morpheus.Client
  ( request,
    single,
  )
import Data.Time.Format (defaultTimeLocale, formatTime)
import EulerHS.Prelude hiding (id, product)
import qualified Kernel.External.MultiModal.Interface.Types as TP
import Kernel.External.MultiModal.OpenTripPlanner.Config
import Kernel.External.MultiModal.OpenTripPlanner.Types
import Kernel.External.MultiModal.Utils
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics, addOpenTripPlannerLatency, addOpenTripPlannerResponse)
import Kernel.Utils.Common hiding (id)
import Servant.Client.Core (showBaseUrl)

formatUtcDateTime :: UTCTime -> (String, String)
formatUtcDateTime utcTime = (dateString, timeString)
  where
    dateString = formatTime defaultTimeLocale "%Y-%m-%d" utcTime
    timeString = formatTime defaultTimeLocale "%H:%M" utcTime

getTransitRoutes ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m
  ) =>
  OTPCfg ->
  TP.GetTransitRoutesReq ->
  m (Maybe TP.MultiModalResponse)
getTransitRoutes cfg req = do
  let origin =
        InputCoordinates
          { lat = req.origin.location.latLng.latitude,
            lon = req.origin.location.latLng.longitude
          }
  let destination =
        InputCoordinates
          { lat = req.destination.location.latLng.latitude,
            lon = req.destination.location.latLng.longitude
          }
  let dateTime' = req.departureTime <&> formatUtcDateTime
  dateTime <-
    case cfg.nightMode of
      Just True -> Just . formatUtcDateTime . addUTCTime 43200 <$> getCurrentTime
      _ -> pure dateTime'
  let planClient = fromString (showBaseUrl cfg.baseUrl)
  let transportModes' = req.transportModes
  let numItineraries' = Just $ fromMaybe 50 cfg.numItineraries
  let minimumWalkDistance = req.minimumWalkDistance
  let permissibleModes = req.permissibleModes
  let maxAllowedPublicTransportLegs = req.maxAllowedPublicTransportLegs
  let sortingType = req.sortingType
  let queryType = fromMaybe NORMAL cfg.queryType
  case queryType of
    NORMAL -> do
      let otpReq =
            OTPPlanArgs
              { from = origin,
                to = destination,
                date = fst <$> dateTime,
                time = snd <$> dateTime,
                transportModes = transportModes',
                numItineraries = numItineraries',
                searchWindow = req.searchWindow,
                noOptimization = req.noOptimization
              }
      (resp, latency) <-
        measureDuration $
          liftIO $
            planClient
              `request` otpReq
                >>= single
      case resp of
        Left err -> do
          logError $ "Error in getTransitRoutes: " <> show err
          addOpenTripPlannerResponse "NORMAL" "FAILURE" "GRAPHQL_ERROR"
          addOpenTripPlannerLatency "NORMAL" "FAILURE" latency
          pure Nothing
        Right plan' -> do
          logInfo $ "OTP plan log by gentleman and piyush: " <> show plan' <> " " <> show req <> " , GQLReq => " <> show otpReq
          addOpenTripPlannerLatency "NORMAL" "SUCCESS" latency
          pure $ Just $ convertOTPToGeneric plan' minimumWalkDistance permissibleModes maxAllowedPublicTransportLegs sortingType cfg.weightedSortCfg
    MULTI_SEARCH -> withLogTag "MULTI_SEARCH" $ do
      let otpReq =
            MultiModePlanArgs
              { from = origin,
                to = destination,
                date = fst <$> dateTime,
                time = snd <$> dateTime,
                metroTransportModes = map (Just . modeToTransportMode) $ catMaybes [Just ModeRAIL, Just ModeWALK],
                metroItineraries = 5,
                subwayTransportModes = map (Just . modeToTransportMode) $ catMaybes [Just ModeSUBWAY, Just ModeWALK],
                subwayItineraries = 5,
                busTransportModes = map (Just . modeToTransportMode) $ catMaybes [Just ModeBUS, Just ModeWALK],
                busItineraries = 10,
                bestTransportModes = map (Just . modeToTransportMode) $ catMaybes [Just ModeTRANSIT, Just ModeWALK],
                bestItineraries = 10,
                searchWindow = req.searchWindow,
                busSearchWindow = req.busSearchWindow,
                noOptimization = req.noOptimization
              }
      (resp, latency) <-
        measureDuration $
          liftIO $
            planClient
              `request` otpReq
                >>= single
      case resp of
        Left err -> do
          logError $ "Error in getTransitRoutes: " <> show err
          addOpenTripPlannerResponse "MULTI_SEARCH" "FAILURE" "GRAPHQL_ERROR"
          addOpenTripPlannerLatency "MULTI_SEARCH" "FAILURE" latency
          pure Nothing
        Right plan -> do
          logInfo $ "OTP plan log: " <> show plan <> " " <> show req <> " , GQLReq => " <> show otpReq
          let allPlans = combinePlans plan
          addOpenTripPlannerLatency "MULTI_SEARCH" "SUCCESS" latency
          pure $ Just $ convertOTPToGeneric allPlans minimumWalkDistance permissibleModes maxAllowedPublicTransportLegs sortingType cfg.weightedSortCfg

modeToTransportMode :: Mode -> TransportMode
modeToTransportMode = TransportMode . show

combinePlans :: MultiModePlan -> OTPPlan
combinePlans res = do
  let itineraries = res.metro.itineraries <> res.subway.itineraries <> res.bus.itineraries <> res.best.itineraries
  OTPPlan {plan = OTPPlanPlan {..}}
