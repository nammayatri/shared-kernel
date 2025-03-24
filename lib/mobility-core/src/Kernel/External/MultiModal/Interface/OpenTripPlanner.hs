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
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
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
  let dateTime = req.departureTime <&> formatUtcDateTime
  let planClient = fromString (showBaseUrl cfg.baseUrl)
  let transportModes' = req.transportModes
  let numItineraries' = Just 25
  let minimumWalkDistance = req.minimumWalkDistance
  let permissibleModes = req.permissibleModes
  let maxAllowedPublicTransportLegs = req.maxAllowedPublicTransportLegs
  let sortingType = req.sortingType
  resp <-
    liftIO $
      planClient
        `request` OTPPlanArgs
          { from = origin,
            to = destination,
            date = fst <$> dateTime,
            time = snd <$> dateTime,
            transportModes = transportModes',
            numItineraries = numItineraries'
          }
          >>= single
  case resp of
    Left _ -> pure Nothing
    Right plan' ->
      pure $ Just $ convertOTPToGeneric plan' minimumWalkDistance permissibleModes maxAllowedPublicTransportLegs sortingType
