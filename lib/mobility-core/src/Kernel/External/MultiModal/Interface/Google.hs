module Kernel.External.MultiModal.Interface.Google (getTransitRoutes) where

import Data.Time
import Kernel.External.Encryption
import Kernel.External.Maps.Google.Config
import qualified Kernel.External.Maps.Google.MapsClient as GoogleMaps
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.External.MultiModal.Utils
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common hiding (id)

formatUtcTime :: Maybe UTCTime -> Maybe String
formatUtcTime Nothing = Nothing
formatUtcTime (Just utcTime) = Just $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" utcTime

getTransitRoutes ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m
  ) =>
  GoogleCfg ->
  MultiModalTypes.GetTransitRoutesReq ->
  m (Maybe MultiModalTypes.MultiModalResponse)
getTransitRoutes cfg req = do
  key <- decrypt cfg.googleKey
  let googleMapsUrl = cfg.googleRouteConfig.url
      computeAlternativeRoutes = cfg.googleRouteConfig.computeAlternativeRoutes
      routePreference = cfg.googleRouteConfig.routePreference
      origin = req.origin
      destination = req.destination
      travelMode = req.mode
      arrivalTime = formatUtcTime req.arrivalTime
      departureTime = formatUtcTime req.departureTime
      transitPreferences = req.transitPreferences
  result <- try @_ @SomeException $ GoogleMaps.transitDirectionsAPI googleMapsUrl key origin destination travelMode computeAlternativeRoutes routePreference transitPreferences arrivalTime departureTime
  case result of
    Right gRes -> do
      pure $ Just $ convertGoogleToGeneric gRes
    Left _ -> do
      pure Nothing
