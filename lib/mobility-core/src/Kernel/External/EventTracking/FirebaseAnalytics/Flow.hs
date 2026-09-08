{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.EventTracking.FirebaseAnalytics.Flow
  ( pushEvent,
    toMpCollectReq,
    selectApp,
    limitWarnings,
    toMicros,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import EulerHS.Types (client)
import Kernel.External.Encryption (decrypt)
import Kernel.External.EventTracking.FirebaseAnalytics.API
import Kernel.External.EventTracking.FirebaseAnalytics.Config
import Kernel.External.EventTracking.FirebaseAnalytics.Types
import Kernel.External.EventTracking.Interface.Types (EventTrackingReq (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Version (DeviceType)
import Kernel.Utils.Common

-- | Push an event via the GA4 Measurement Protocol. Skips when the rider has no
-- app instance id or platform, or no app is configured for the platform.
pushEvent ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  FirebaseAnalyticsCfg ->
  EventTrackingReq ->
  m ()
pushEvent cfg req =
  case (req.appInstanceId, req.platform) of
    (Nothing, _) ->
      logDebug $ "FirebaseAnalytics: no app instance id for customer " <> req.customerId <> ", skipping event " <> req.eventName
    (_, Nothing) ->
      logWarning $ "FirebaseAnalytics: no client platform for customer " <> req.customerId <> ", skipping event " <> req.eventName
    (Just appInstanceId, Just platform) ->
      case selectApp platform cfg.apps of
        Nothing ->
          logWarning $ "FirebaseAnalytics: no app configured for platform " <> show platform <> ", skipping event " <> req.eventName
        Just firebaseApp -> do
          forM_ (limitWarnings req) $ \warning -> logWarning $ "FirebaseAnalytics: " <> warning
          apiSecret <- decrypt firebaseApp.apiSecret
          let body = toMpCollectReq appInstanceId req
          if cfg.debug
            then validateEvent cfg firebaseApp apiSecret body
            else sendEvent cfg firebaseApp apiSecret body

sendEvent ::
  ( CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  FirebaseAnalyticsCfg ->
  FirebaseAppCfg ->
  Text ->
  MpCollectReq ->
  m ()
sendEvent cfg firebaseApp apiSecret body = do
  let mpClient = client (Proxy :: Proxy MpCollectAPI)
  void $
    callAPI cfg.baseUrl (mpClient firebaseApp.firebaseAppId apiSecret body) "firebaseMpCollect" mpCollectAPI
      >>= fromEitherM (\err -> InternalError $ "Failed to call Firebase Measurement Protocol: " <> redactClientError (show err))
  logDebug $ "FirebaseAnalytics event " <> eventNames body <> " sent to app " <> firebaseApp.firebaseAppId

validateEvent ::
  ( CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  FirebaseAnalyticsCfg ->
  FirebaseAppCfg ->
  Text ->
  MpCollectReq ->
  m ()
validateEvent cfg firebaseApp apiSecret body = do
  let mpClient = client (Proxy :: Proxy MpDebugCollectAPI)
  resp <-
    callAPI cfg.baseUrl (mpClient firebaseApp.firebaseAppId apiSecret body) "firebaseMpDebugCollect" mpDebugCollectAPI
      >>= fromEitherM (\err -> InternalError $ "Failed to call Firebase Measurement Protocol validation endpoint: " <> redactClientError (show err))
  case resp.validationMessages of
    [] ->
      logInfo $ "FirebaseAnalytics (debug): event " <> eventNames body <> " is valid for app " <> firebaseApp.firebaseAppId <> "; not recorded"
    messages ->
      forM_ messages $ \message ->
        logWarning $
          "FirebaseAnalytics (debug): event " <> eventNames body <> " rejected: "
            <> fromMaybe "UNKNOWN" message.validationCode
            <> " at "
            <> fromMaybe "-" message.fieldPath
            <> ": "
            <> message.description

eventNames :: MpCollectReq -> Text
eventNames body = T.intercalate "," (map (.name) body.events)

selectApp :: DeviceType -> [FirebaseAppCfg] -> Maybe FirebaseAppCfg
selectApp platform = listToMaybe . filter ((== platform) . (.platform))

toMpCollectReq :: Text -> EventTrackingReq -> MpCollectReq
toMpCollectReq appInstanceId req =
  MpCollectReq
    { app_instance_id = appInstanceId,
      user_id = Just req.customerId,
      timestamp_micros = toMicros <$> req.timestamp,
      events =
        [ MpEvent
            { name = req.eventName,
              params = req.attributes
            }
        ]
    }

toMicros :: UTCTime -> Integer
toMicros = floor . (* 1000000) . utcTimeToPOSIXSeconds

-- | Google drops or truncates silently past these limits; we only warn.
limitWarnings :: EventTrackingReq -> [Text]
limitWarnings req =
  catMaybes
    [ warnWhen (T.length req.eventName > 40) $
        "event name " <> req.eventName <> " exceeds 40 characters; Google will drop it",
      warnWhen (any (`T.isPrefixOf` req.eventName) reservedPrefixes) $
        "event name " <> req.eventName <> " uses a reserved prefix; Google will drop it",
      case req.attributes of
        A.Object params
          | length params > 25 ->
            Just $ "event " <> req.eventName <> " has " <> show (length params) <> " params; Google keeps at most 25"
        _ -> Nothing,
      case req.attributes of
        A.Object params
          | any tooLong params ->
            Just $ "event " <> req.eventName <> " has a string param over 100 characters; Google will truncate or drop it"
        _ -> Nothing,
      case req.attributes of
        A.Object params
          | any ((> 40) . T.length . AK.toText) (AKM.keys params) ->
            Just $ "event " <> req.eventName <> " has a param name over 40 characters; Google will drop it"
        _ -> Nothing
    ]
  where
    warnWhen condition message = if condition then Just message else Nothing
    reservedPrefixes = ["_", "firebase_", "ga_", "google_"]
    tooLong (A.String value) = T.length value > 100
    tooLong _ = False
