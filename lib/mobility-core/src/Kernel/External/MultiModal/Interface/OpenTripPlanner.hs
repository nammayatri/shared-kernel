module Kernel.External.MultiModal.Interface.OpenTripPlanner (getTransitRoutes) where

import Data.Morpheus.Client
  ( GQLClient,
    ResponseStream,
    request,
    single,
  )
import Data.Time.Format (defaultTimeLocale, formatTime)
-- import Kernel.Utils.Time (diffUTCTime)
-- import Kernel.Types.Forkable (Forkable, awaitableFork)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, product)
import qualified Kernel.External.MultiModal.Interface.Types as TP
import Kernel.External.MultiModal.OpenTripPlanner.Config
import Kernel.External.MultiModal.OpenTripPlanner.Types
import Kernel.External.MultiModal.Utils
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics, addOpenTripPlannerLatency, addOpenTripPlannerResponse)
import qualified Kernel.Types.Distance as Distance
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
    Log m,
    Forkable m
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
                numItineraries = numItineraries'
              }
      sendNormalOTPRequest planClient otpReq "NORMAL" minimumWalkDistance permissibleModes maxAllowedPublicTransportLegs sortingType cfg.weightedSortCfg
    MULTI_SEARCH -> withLogTag "MULTI_SEARCH" $ do
      case req.transportModes of
        Just _ -> do
          let otpReq =
                OTPPlanArgs
                  { from = origin,
                    to = destination,
                    date = fst <$> dateTime,
                    time = snd <$> dateTime,
                    transportModes = req.transportModes,
                    numItineraries = numItineraries'
                  }
          sendNormalOTPRequest planClient otpReq "MULTI_SEARCH" minimumWalkDistance permissibleModes maxAllowedPublicTransportLegs sortingType cfg.weightedSortCfg
        Nothing -> do
          let metroReq =
                OTPPlanArgs
                  { from = origin,
                    to = destination,
                    date = fst <$> dateTime,
                    time = snd <$> dateTime,
                    transportModes = Just $ map (Just . TransportMode) [ModeRAIL, ModeWALK],
                    numItineraries = Just 5
                  }
          let subwayReq =
                metroReq
                  { transportModes = Just $ map (Just . TransportMode) [ModeSUBWAY, ModeWALK],
                    numItineraries = Just 5
                  }
          let busReq =
                metroReq
                  { transportModes = Just $ map (Just . TransportMode) [ModeBUS, ModeWALK],
                    numItineraries = Just 10
                  }
          let bestReq =
                metroReq
                  { transportModes = Just $ map (Just . TransportMode) [ModeTRANSIT, ModeWALK],
                    numItineraries = Just 10
                  }
          startTime <- getCurrentTime
          metroAwaitable <- awaitableFork "metro-query" $ liftIO $ (requestPlan planClient metroReq) >>= single
          subwayAwaitable <- awaitableFork "subway-query" $ liftIO $ (requestPlan planClient subwayReq) >>= single
          busAwaitable <- awaitableFork "bus-query" $ liftIO $ (requestPlan planClient busReq) >>= single
          bestAwaitable <- awaitableFork "best-query" $ liftIO $ (requestPlan planClient bestReq) >>= single
          metroResult <- L.await Nothing metroAwaitable
          subwayResult <- L.await Nothing subwayAwaitable
          busResult <- L.await Nothing busAwaitable
          bestResult <- L.await Nothing bestAwaitable
          endTime <- getCurrentTime
          let totalLatency = secondsToMillis $ nominalDiffTimeToSeconds $ diffUTCTime endTime startTime
          let extractItineraries result = case result of
                Right (Right plan) -> Just (plan.plan.itineraries)
                Right (Left _) -> Nothing
                Left _ -> Nothing
          let successfulItineraries =
                concat $
                  catMaybes
                    [ extractItineraries metroResult,
                      extractItineraries subwayResult,
                      extractItineraries busResult,
                      extractItineraries bestResult
                    ]
          if null successfulItineraries
            then do
              logError "All MULTI_SEARCH queries failed"
              addOpenTripPlannerResponse "MULTI_SEARCH" "FAILURE" "ALL_QUERIES_FAILED"
              addOpenTripPlannerLatency "MULTI_SEARCH" "FAILURE" totalLatency
              pure Nothing
            else do
              when (length successfulItineraries < 4) $
                logWarning $
                  "Some MULTI_SEARCH queries failed, returning partial results: "
                    <> show (length successfulItineraries)
                    <> " itineraries"

              let combinedPlan = OTPPlan {plan = OTPPlanPlan {itineraries = successfulItineraries}}

              addOpenTripPlannerLatency "MULTI_SEARCH" "PARTIAL_SUCCESS" totalLatency
              pure $
                Just $
                  convertOTPToGeneric
                    combinedPlan
                    minimumWalkDistance
                    permissibleModes
                    maxAllowedPublicTransportLegs
                    sortingType
                    cfg.weightedSortCfg

requestPlan :: GQLClient -> OTPPlanArgs -> IO (ResponseStream OTPPlan)
requestPlan planClient args = planClient `request` args

sendNormalOTPRequest ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m,
    Forkable m
  ) =>
  GQLClient ->
  OTPPlanArgs ->
  Text ->
  Distance.Meters ->
  [TP.GeneralVehicleType] ->
  Int ->
  TP.SortingType ->
  MultiModalWeightedSortCfg ->
  m (Maybe TP.MultiModalResponse)
sendNormalOTPRequest planClient otpReq label minimumWalkDistance permissibleModes maxAllowedPublicTransportLegs sortingType weightedSortCfg = do
  (resp, latency) <-
    measureDuration $
      liftIO $
        requestPlan planClient otpReq >>= single
  case resp of
    Left err -> do
      logError $ "Error in getTransitRoutes: " <> show err
      addOpenTripPlannerResponse label "FAILURE" "GRAPHQL_ERROR"
      addOpenTripPlannerLatency label "FAILURE" latency
      pure Nothing
    Right plan' -> do
      addOpenTripPlannerLatency label "SUCCESS" latency
      pure $ Just $ convertOTPToGeneric plan' minimumWalkDistance permissibleModes maxAllowedPublicTransportLegs sortingType weightedSortCfg
