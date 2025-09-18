module Kernel.External.MultiModal.Interface.OpenTripPlanner (getTransitRoutes) where

import Data.Morpheus.Client
  ( GQLClient,
    GQLClientResult,
    ResponseStream,
    request,
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
import Kernel.Utils.Forkable
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
          addOpenTripPlannerLatency "NORMAL" "SUCCESS" latency
          pure $ Just $ convertOTPToGeneric plan' minimumWalkDistance permissibleModes maxAllowedPublicTransportLegs sortingType cfg.weightedSortCfg
    MULTI_SEARCH -> withLogTag "MULTI_SEARCH" $ do
      let requests =
            [ ("metro-query", mkReq origin destination dateTime [ModeRAIL, ModeWALK] 5),
              ("subway-query", mkReq origin destination dateTime [ModeSUBWAY, ModeWALK] 5),
              ("bus-query", mkReq origin destination dateTime [ModeBUS, ModeWALK] 10),
              ("best-query", mkReq origin destination dateTime [ModeTRANSIT, ModeWALK] 10)
            ]
      start <- getClockTimeInMs
      results <-
        mapConcurrentlyTagged
          (\reqArgs -> liftIO $ requestPlan planClient reqArgs >>= single)
          requests
      end <- getClockTimeInMs
      let latency = end - start
      let anyFailed = any isLeft results
      let allItineraries = mapMaybe extractItineraries results
      let successfulItineraries = concat allItineraries
      when anyFailed $ do
        logError "MULTI_SEARCH query failed"
        addOpenTripPlannerResponse "MULTI_SEARCH" "FAILURE" "GRAPHQL_ERROR"
      if null allItineraries
        then do
          addOpenTripPlannerLatency "MULTI_SEARCH" "FAILURE" latency
          pure Nothing
        else do
          addOpenTripPlannerLatency "MULTI_SEARCH" "SUCCESS" latency
          let combinedPlan = OTPPlan {plan = OTPPlanPlan {itineraries = successfulItineraries}}
          pure $
            Just $
              convertOTPToGeneric
                combinedPlan
                minimumWalkDistance
                permissibleModes
                maxAllowedPublicTransportLegs
                sortingType
                cfg.weightedSortCfg
  where
    mkReq :: InputCoordinates -> InputCoordinates -> Maybe (String, String) -> [Mode] -> Int -> OTPPlanArgs
    mkReq origin destination dateTime modes n =
      OTPPlanArgs
        { from = origin,
          to = destination,
          date = fst <$> dateTime,
          time = snd <$> dateTime,
          transportModes = Just $ map (Just . modeToTransportMode) modes,
          numItineraries = Just n
        }
    extractItineraries :: GQLClientResult OTPPlan -> Maybe [Maybe OTPPlanPlanItineraries]
    extractItineraries result = case result of
      Right plan -> Just plan.plan.itineraries
      _ -> Nothing

    modeToTransportMode :: Mode -> TransportMode
    modeToTransportMode = TransportMode . show

    requestPlan :: GQLClient -> OTPPlanArgs -> IO (ResponseStream OTPPlan)
    requestPlan planClient args = planClient `request` args
