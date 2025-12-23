{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : FCM.Flow
-- Description : Firebase Cloud Messaging module
--
-- FCM description: https://firebase.google.com/docs/cloud-messaging
-- Firebase Cloud Messaging (FCM) is a cross-platform messaging solution
-- that lets you reliably send messages at no cost. Using FCM, you can notify
-- a client app that new email or other data is available to sync. You can
-- send notification messages to drive user re-engagement and retention.
-- For use cases such as instant messaging, a message can transfer
-- a payload of up to 4KB to a client app.
--
-- Protocol description : https://firebase.google.com/docs/reference/fcm/rest/v1/projects.messages
module Kernel.External.Notification.FCM.Flow
  ( createMessage,
    createAndroidNotification,
    notifyPerson,
    notifyPersonWithPriority,
    FCMSendMessageAPI,
    fcmSendMessageAPI,
    parseFCMAccount,
    createAndroidNotificationWithIcon,
    createAndroidOverlayNotification,
    ApnsLiveActivityAPI,
    apnsLiveActivityAPI,
    updateLiveActivity,
    createApnsLiveActivtyPayload,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Default.Class
import qualified Data.Text as T'
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Base64 as B64
import Data.Time.Clock.POSIX (getPOSIXTime)
import EulerHS.Prelude hiding ((^.))
import qualified EulerHS.Types as ET
import Kernel.External.Notification.FCM.Error
import Kernel.External.Notification.FCM.Types
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Kernel.Utils.JWT as JWT
import Servant
import Servant.Client (ClientError (..), ResponseF (..))

-- | Create FCM message
-- Note that data should be formed as key-value pairs list
-- recipientId::FCMToken is an app's registration token
createMessage :: FCMData a -> FCMRecipientToken -> Maybe FCMAndroidMessagePriority -> (FCMData a -> FCMData b) -> FCMMessage a b
createMessage msgData recipientId priority iosModifier =
  def{fcmToken = Just recipientId,
      fcmAndroid = Just androidCfg,
      fcmApns = Just apnsCfg
     }
  where
    androidCfg = createAndroidConfig msgData priority
    apnsCfg = createApnsConfig msgData iosModifier

-- | Android Notification details
createAndroidConfig :: FCMData a -> Maybe FCMAndroidMessagePriority -> FCMAndroidConfig a
createAndroidConfig cfgData priority =
  def{fcmdData = Just cfgData,
      fcmdPriority = priority
     }

createApnsConfig :: FCMData a -> (FCMData a -> FCMData b) -> FCMApnsConfig b
createApnsConfig androidFcmData iosModifier =
  def{fcmaPayload = Just apnsPayload,
      fcmaHeaders =
        Just
          ( def{fcmApnsPriority = Just "10"
               }
          )
     }
  where
    apnsPayload = createApnsPayload androidFcmData iosModifier

createApnsPayload :: forall a b. FCMData a -> (FCMData a -> FCMData b) -> FCMApnPayload b
createApnsPayload androidData iosModifier =
  def {fcmAps = Just fcmAps}
  where
    fcmAlert :: FCMAlert
    fcmAlert =
      def{fcmBody = (.getFCMNotificationBody) <$> body,
          fcmTitle = (.getFCMNotificationTitle) <$> title
         }
    fcmAps :: FCMaps b
    fcmAps =
      def{fcmAlert = if androidData.fcmShowNotification == SHOW then Just fcmAlert else Nothing,
          fcmData = Just (iosModifier androidData),
          fcmCategory = Just androidData.fcmNotificationType,
          fcmMutableContent = 1,
          fcmSound = Just $ fromMaybe "" androidData.fcmNotificationJSON.fcmdSound,
          fcmContentAvailable = 0,
          fcmBadge = if androidData.fcmShowNotification == DO_NOT_SHOW then Just 0 else Nothing
         }
    title :: Maybe FCMNotificationTitle
    title = androidData.fcmNotificationJSON.fcmdTitle

    body :: Maybe FCMNotificationBody
    body = androidData.fcmNotificationJSON.fcmdBody

createAndroidNotification :: FCMNotificationTitle -> FCMNotificationBody -> FCMNotificationType -> Maybe Text -> FCMAndroidNotification
createAndroidNotification title body notificationType sound =
  let notification = case notificationType of
        ALLOCATION_REQUEST ->
          def{fcmdChannelId = Just "RINGING_ALERT"
             }
        TRIP_STARTED ->
          def{fcmdChannelId = Just "TRIP_STARTED"
             }
        _ -> def
   in notification
        { fcmdTitle = Just title,
          fcmdBody = Just body,
          fcmdIcon =
            Just $
              FCMNotificationIconUrl
                "http://localhost:8080/static/images/ride-success.png",
          fcmdTag = Just notificationType,
          fcmdSound = sound
        }

createAndroidNotificationWithIcon :: FCMNotificationTitle -> FCMNotificationBody -> FCMNotificationType -> Maybe Text -> FCMAndroidNotification
createAndroidNotificationWithIcon title body notificationType mIcon =
  let notification = def
   in notification
        { fcmdTitle = Just title,
          fcmdBody = Just body,
          fcmdIcon = Just $ FCMNotificationIconUrl (fromMaybe "http://localhost:8080/static/images/ride-success.png" mIcon),
          fcmdTag = Just notificationType
        }

createAndroidOverlayNotification ::
  FCMOverlayReq ->
  FCMOverlayNotificationJSON
createAndroidOverlayNotification _req@FCMOverlayReq {..} =
  FCMOverlayNotificationJSON
    { titleVisibility = isJust title,
      descriptionVisibility = isJust description,
      buttonOkVisibility = isJust okButtonText,
      buttonCancelVisibility = isJust cancelButtonText,
      buttonLayoutVisibility = isJust okButtonText || isJust cancelButtonText,
      imageVisibility = isJust imageUrl,
      ..
    }

-- | Send FCM message to a person
notifyPerson ::
  ( CoreMetrics m,
    ToJSON a,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  FCMConfig ->
  m () ->
  FCMData a ->
  FCMNotificationRecipient ->
  m ()
notifyPerson config action msgData recipient = notifyPersonWithPriority config Nothing action msgData recipient EulerHS.Prelude.id

notifyPersonWithPriority ::
  ( CoreMetrics m,
    ToJSON a,
    ToJSON b,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  FCMConfig ->
  Maybe FCMAndroidMessagePriority ->
  m () ->
  FCMData a ->
  FCMNotificationRecipient ->
  (FCMData a -> FCMData b) ->
  m ()
notifyPersonWithPriority config priority action msgData recipient iosModifier = do
  let tokenNotFound = "device token of a person " <> recipient.id <> " not found"
  case recipient.token of
    Nothing -> do
      logTagInfo "FCM" tokenNotFound
      pure ()
    Just token -> sendMessage config (FCMRequest (createMessage msgData token priority iosModifier)) action recipient.id

-- | Google API interface
type FCMSendMessageAPI a b =
  Header "Authorization" FCMAuthToken
    :> ReqBody '[JSON] (FCMRequest a b)
    :> Post '[JSON] FCMResponse

fcmSendMessageAPI :: Proxy (FCMSendMessageAPI a b)
fcmSendMessageAPI = Proxy

-- | Send FCM message to a registered device
sendMessage ::
  ( CoreMetrics m,
    ToJSON a,
    ToJSON b,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  FCMConfig ->
  FCMRequest a b ->
  m () ->
  Text ->
  m ()
sendMessage config fcmMsg action toWhom = fork desc $ do
  logTagInfo fcm $ "Message to be sent to the person: " <> show (Aeson.encode fcmMsg)
  authToken <- getTokenText config
  case authToken of
    Right token -> do
      let fcmUrl = config.fcmUrl
      res <- callAPI fcmUrl (callFCM (Just $ FCMAuthToken token) fcmMsg) "sendMessage" fcmSendMessageAPI
      case res of
        Right _ -> logTagInfo fcm $ "Message sent successfully to a person with id " <> toWhom
        Left clientError -> do
          case clientError of
            FailureResponse _ (Response _ _ _ resbody) -> do
              let eitherError = Aeson.eitherDecodeStrict (BL.toStrict resbody) :: Either String FcmError
              case eitherError of
                Right fcmError -> handleFcmError fcmError action
                Left errorMsg -> logTagError fcm $ "FCM decoding failed for person with id : " <> toWhom <> " Error Message : " <> T'.pack errorMsg
            _ -> return ()
    Left err -> logTagError fcm $ "AuthToken error while sending message to person with id " <> toWhom <> " : " <> show err
  where
    callFCM token msg = void $ ET.client fcmSendMessageAPI token msg
    desc = "FCM send message forked flow"
    fcm = "FCM"

    handleFcmError :: MonadFlow m => FcmError -> m () -> m ()
    handleFcmError (FcmError (Just (ErrorRes _ _ _ (Just details)))) action' =
      mapM_ (`handleDetail` action') details
    handleFcmError _ _ = pure ()

    handleDetail :: MonadFlow m => ErrorDetail -> m () -> m ()
    handleDetail (ErrorDetail (Just errorCode)) action' =
      case errorCode of
        "UNREGISTERED" -> do
          logTagError fcm $ "Error while sending message to person with id " <> toWhom <> " : " <> "device token is unregistered and errorCode is : " <> show errorCode
          action'
        _ -> logTagError fcm $ "Error while sending message to person with id " <> toWhom <> " : " <> "unknown error code " <> show errorCode
    handleDetail _ _ = pure ()

-- | try to get FCM text token
getTokenText ::
  (Redis.HedisFlow m r, MonadFlow m, HasRequestId r, MonadReader r m) =>
  FCMConfig ->
  m (Either Text Text)
getTokenText config = do
  token <- getToken config
  pure $ case token of
    Left err -> Left $ fromString err
    Right t -> Right $ JWT.jwtTokenType t <> " " <> JWT.jwtAccessToken t

redisFcmKey :: Text
redisFcmKey = "mobility:fcm_token"

-- | Get token (refresh token if expired / invalid)
getToken ::
  (Redis.HedisFlow m r, MonadFlow m, HasRequestId r, MonadReader r m) =>
  FCMConfig ->
  m (Either String JWT.JWToken)
getToken config = do
  tokenStatus <-
    Redis.get (config.fcmTokenKeyPrefix <> ":" <> redisFcmKey) >>= \case
      Nothing -> pure $ Left "Token not found"
      Just jwt -> do
        validityStatus <- liftIO $ JWT.isValid jwt
        pure $ case validityStatus of
          JWT.JWTValid _ -> Right jwt
          JWT.JWTExpired _ -> Left "Token expired"
          JWT.JWTInvalid -> Left "Token is invalid"
  case tokenStatus of
    Left err -> do
      logTagWarning "FCM" $ "Refreshing FCM token. Reason: " <> fromString err
      getNewToken config
    jwt -> pure jwt

parseFCMAccount ::
  Text ->
  Either String JWT.ServiceAccount
parseFCMAccount fcmServiceAccount = do
  case BL.fromStrict . T.encodeUtf8 <$> B64.decodeBase64 fcmServiceAccount of
    Right bs -> Aeson.eitherDecode bs
    _ -> Left "FCM JSON file is not set in configs"

getNewToken :: (Redis.HedisFlow m r, MonadFlow m, HasRequestId r, MonadReader r m) => FCMConfig -> m (Either String JWT.JWToken)
getNewToken config = either (pure . Left) (refreshToken config) $ parseFCMAccount config.fcmServiceAccount

refreshToken :: (Redis.HedisFlow m r, MonadFlow m, HasRequestId r, MonadReader r m) => FCMConfig -> JWT.ServiceAccount -> m (Either String JWT.JWToken)
refreshToken config fcmAcc = do
  logTagInfo fcmTag "Refreshing token"
  refreshRes <- liftIO $ JWT.doRefreshToken fcmAcc
  case refreshRes of
    Left err -> do
      logTagInfo fcmTag $ fromString err
      pure $ Left $ fromString err
    Right token -> do
      logTagInfo fcmTag $ fromString "Success"
      Redis.set (config.fcmTokenKeyPrefix <> ":" <> redisFcmKey) token
      pure $ Right token
  where
    fcmTag = "FCM"

updateLiveActivity ::
  ( CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  FCMConfig ->
  FCMNotificationRecipient ->
  LiveActivityReq ->
  m ()
updateLiveActivity config recipient apnsReq = do
  let tokenNotFound = "device token of a person " <> recipient.id <> " not found"
  case recipient.token of
    Nothing -> do
      logTagInfo "FCM" tokenNotFound
      pure ()
    Just token -> do
      currentTime <- liftIO getPOSIXTime
      let currentTimeInt = floor currentTime :: Int
          apnsReqTimeStamp = currentTimeInt
      sendLiveActivityApns config (createApnsLiveActivtyPayload token apnsReq apnsReqTimeStamp) recipient.id

sendLiveActivityApns ::
  ( CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  FCMConfig ->
  ApnsAPIRequest ->
  Text ->
  m ()
sendLiveActivityApns config apnsApiRequest toWhom = do
  authToken <- getTokenText config
  case authToken of
    Right token -> do
      let fcmUrl = config.fcmUrl
      res <- callAPI fcmUrl (callAPNS (Just $ FCMAuthToken token) apnsApiRequest) "sendAPNSPayload" apnsLiveActivityAPI
      case res of
        Right _ -> logTagInfo apns $ "APNS sent successfully to a person with id " <> toWhom
        Left _ -> logTagInfo apns "APNS WAS NOT SEND"
    Left err -> logTagError apns $ "AuthToken error while sending apns to person with id " <> toWhom <> " : " <> show err
  where
    callAPNS token req = void $ ET.client apnsLiveActivityAPI token req
    apns = "APNS"

-- | Google API interface
type ApnsLiveActivityAPI req =
  Header "Authorization" FCMAuthToken
    :> ReqBody '[JSON] req
    :> Post '[JSON] ResponseType -- APNS Response

data ResponseType = ResponseType
  { name :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

apnsLiveActivityAPI :: Proxy (ApnsLiveActivityAPI a)
apnsLiveActivityAPI = Proxy

createApnsLiveActivtyPayload :: FCMRecipientToken -> LiveActivityReq -> Int -> ApnsAPIRequest
createApnsLiveActivtyPayload receipentToken apnsReq apnsReqTimeStamp =
  let apnsReqToken = apnsReq.liveActivityToken
      apnsReqLiveActivity = apnsReq.liveActivityReqType
      apnsContentState = apnsReq.liveActivityContentState
      apnsReqDismissalDate = case apnsReq.liveActivityReqType of
        "end" -> Just $ apnsReqTimeStamp + 600
        _ -> Just $ apnsReqTimeStamp
      liveActivityApnsPriority = apnsReq.liveActivityApnsPriority
      apnsPayload' =
        ApnsAPIRequest
          { message =
              Message
                { token = receipentToken,
                  apns =
                    Apns
                      { live_activity_token =
                          apnsReqToken,
                        headers =
                          ApnsHeaders
                            { apns_priority = liveActivityApnsPriority
                            },
                        payload =
                          Payload
                            { aps =
                                Aps
                                  { timestamp = apnsReqTimeStamp,
                                    content_available = 1,
                                    event = apnsReqLiveActivity,
                                    content_state = apnsContentState,
                                    alert =
                                      Just $
                                        Alert
                                          { title = Just "LIVE"
                                          },
                                    dismissal_date = apnsReqDismissalDate
                                  }
                            }
                      }
                }
          }
   in apnsPayload'
