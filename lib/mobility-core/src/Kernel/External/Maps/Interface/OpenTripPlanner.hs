module Kernel.External.Maps.Interface.OpenTripPlanner (getTransitRoutes) where

import Data.Morpheus.Client
  ( request,
    single,
  )
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import EulerHS.Prelude hiding (id, product)
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Interface.Types as TP
import Kernel.External.Maps.OpenTripPlanner.Config
import Kernel.External.Maps.OpenTripPlanner.Types
import Kernel.External.MultiModal.Types
import Kernel.External.MultiModal.Utils
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common hiding (id)

convertDateTime :: String -> Maybe (String, String)
convertDateTime input = do
  time <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" input :: Maybe UTCTime
  let dateStr = formatTime defaultTimeLocale "%Y-%m-%d" time
  let timeStr = formatTime defaultTimeLocale "%H:%M" time
  return (dateStr, timeStr)

getTransitRoutes ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m
  ) =>
  OTPCfg ->
  TP.GetTransitRoutesReq ->
  m (Maybe MultiModalResponse)
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
  let dateTime = req.arrivalTime >>= convertDateTime
  let planClient = fromString cfg.baseUrl
  let transportModes' = req.transportModes
  resp <-
    liftIO $
      planClient
        `request` OTPPlanArgs
          { from = origin,
            to = destination,
            date = fst <$> dateTime,
            time = snd <$> dateTime,
            transportModes = transportModes'
          }
          >>= single
  case resp of
    Left _ -> pure Nothing
    Right plan' ->
      pure $ Just $ convertOTPToGeneric plan'
