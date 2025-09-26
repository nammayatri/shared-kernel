{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Notification.FCM.Types where

import Control.Lens.TH
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Default.Class
import Data.OpenApi (ToSchema)
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField)
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Storage.Esqueleto (PersistField, PersistFieldSql)
import Kernel.Types.App
import qualified Kernel.Utils.Common as Common
import Kernel.Utils.GenericPretty
import Kernel.Utils.JSON
import Kernel.Utils.TH
import Kernel.Utils.Text (decodeFromText, encodeToText)

data FCMConfig = FCMConfig
  { fcmUrl :: BaseUrl,
    fcmServiceAccount :: Text,
    fcmTokenKeyPrefix :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyShow, FromJSON, ToJSON)

data FCMNotificationRecipient = FCMNotificationRecipient
  { id :: Text,
    token :: Maybe FCMRecipientToken
  }

-- | Device token
newtype FCMRecipientToken = FCMRecipientToken
  { getFCMRecipientToken :: Text
  }
  deriving newtype (PersistField, PersistFieldSql, Show, PrettyShow)

deriving newtype instance FromField FCMRecipientToken

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be FCMRecipientToken where
  sqlValueSyntax = sqlValueSyntax . getFCMRecipientToken

instance BeamSqlBackend be => B.HasSqlEqualityCheck be FCMRecipientToken

instance FromBackendRow Postgres FCMRecipientToken

deriving stock instance Read FCMRecipientToken

deriveIdentifierInstances ''FCMRecipientToken

-- | FCM authorization token
newtype FCMAuthToken = FCMAuthToken
  { getFCMAuthToken :: Text
  }
  deriving (Show)
  deriving newtype (PrettyShow)

deriveIdentifierInstances ''FCMAuthToken

-- | FCM notification title
newtype FCMNotificationTitle = FCMNotificationTitle
  { getFCMNotificationTitle :: Text
  }
  deriving (Show)
  deriving newtype (PrettyShow)

deriveIdentifierInstances ''FCMNotificationTitle

-- | FCM notification body
newtype FCMNotificationBody = FCMNotificationBody
  { getFCMNotificationBody :: Text
  }
  deriving (Show)
  deriving newtype (PrettyShow)

deriveIdentifierInstances ''FCMNotificationBody

-- | Notification image / icon path
newtype FCMNotificationIconUrl = FCMNotificationIconUrl
  { getFCMNotificationIconUrl :: Text
  }
  deriving (Show)
  deriving newtype (PrettyShow)

deriveIdentifierInstances ''FCMNotificationIconUrl

-- | Notification types
data FCMNotificationType
  = REGISTRATION_APPROVED
  | EXPIRED_CASE
  | CANCELLED_PRODUCT
  | CANCELLED_PRODUCT_DRIVER
  | CANCELLED_PRODUCT_USER
  | REALLOCATE_PRODUCT
  | DRIVER_ASSIGNMENT
  | TRIP_STARTED
  | TRIP_FINISHED
  | ALLOCATION_REQUEST
  | ALLOCATION_REQUEST_UNASSIGNED
  | ACCOUNT_DISABLED
  | TRIGGER_SERVICE
  | FARE_POLICY_CHANGED
  | DISCOUNT_CHANGED
  | QUOTE_RECEIVED
  | NEW_RIDE_AVAILABLE
  | DRIVER_QUOTE_INCOMING
  | DRIVER_ON_THE_WAY
  | DRIVER_HAS_REACHED
  | DRIVER_REACHING
  | CLEARED_FARE
  | CANCELLED_SEARCH_REQUEST
  | NEW_MESSAGE
  | REFERRAL_ACTIVATED
  | CHAT_MESSAGE
  | PAYMENT_PENDING
  | PAYMENT_OVERDUE
  | PAYMENT_FAILED
  | PAYMENT_SUCCESS
  | PAYMENT_MODE_MANUAL
  | PAYMENT_NUDGE
  | DRIVER_NOTIFY
  | DRIVER_NOTIFY_LOCATION_UPDATE
  | SAFETY_ALERT_DEVIATION
  | DRIVER_BIRTHDAY
  | EDIT_LOCATION
  | ADD_STOP
  | EDIT_STOP
  | STOP_REACHED
  | COINS_SUCCESS
  | FOLLOW_RIDE
  | SHARE_RIDE
  | SOS_TRIGGERED
  | SOS_MOCK_DRILL
  | SOS_RESOLVED
  | EMERGENCY_CONTACT_ADDED
  | SOS_MOCK_DRILL_NOTIFY
  | DOCUMENT_INVALID
  | SCHEDULED_RIDE_NOTIFICATION
  | FIRST_RIDE_EVENT
  | TOLL_CROSSED
  | TRIP_UPDATED
  | FCM_CHAT_MESSAGE
  | PAYOUT_REWARD
  | PAYOUT_VPA_REMINDER
  | TRIGGER_FCM
  | POST_RIDE_SOS_ALERT
  | REFERRAL_FLOW
  | FILE_UPLOADED
  | SCHEDULED_RIDE_REMINDER
  | DRIVER_HAS_REACHED_DESTINATION
  | CANCELLATION_RATE_NUDGE_DAILY
  | CANCELLATION_RATE_NUDGE_WEEKLY
  | USER_FAVOURITE_DRIVER
  | DRIVER_UNBLOCKED
  | POST_RIDE_SAFETY_CHECK
  | DRIVER_STOP_DETECTED
  | TO_METRO_COINS
  | FROM_METRO_COINS
  | ISSUE_BREACH_EXTRA_FARE_MITIGATION
  | MARKETING_EVENTS
  | WMB_TRIP_ASSIGNED
  | WMB_TRIP_STARTED
  | WMB_TRIP_FINISHED
  | FLEET_CONSENT
  | OPERATOR_CONSENT
  | DRIVER_UNLINK_FROM_FLEET
  | DRIVER_UNLINK_FROM_OPERATOR
  | FLEET_UNLINK_FROM_OPERATOR
  | DRIVER_REQUEST_REJECTED
  | SAFETY_ALERT_RIDE_STOPPAGE
  | EKD_LIVE_CALL_FEEDBACK
  | DRUNK_AND_DRIVE_VIOLATION_WARNING
  | ACCOUNT_DELETED
  | DRIVER_ACCEPTED_EDITED_LOCATION
  | DRIVER_UNSUBSCRIBED
  | PICKUP_INSTRUCTIONS
  | PREPAID_RECHARGE_SUCCESS
  | PREPAID_BALANCE_UPDATE
  | PAYOUT_INITIATED
  | PAYOUT_FAILED
  | PAYOUT_COMPLETED
  | EXOTEL_CALL_UNREACHABLE
  | END_RIDE_OFFERS
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable FCMNotificationType

$(mkBeamInstancesForEnum ''FCMNotificationType)

-- | Entity types types
data FCMEntityType = SearchRequest | Product | Merchant | Person | PaymentOrder | EditLocation
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable FCMEntityType

-- | Priority of a message to send to Android devices
data FCMAndroidMessagePriority = NORMAL | HIGH
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable FCMAndroidMessagePriority

-- | Priority levels of a notification
data FCMNotificationPriority
  = PRIORITY_UNSPECIFIED
  | PRIORITY_MIN
  | PRIORITY_LOW
  | PRIORITY_DEFAULT
  | PRIORITY_HIGH
  | PRIORITY_MAX
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable FCMNotificationPriority

-- | Different visibility levels of a notification
data FCMNotificationVisibility = VISIBILITY_UNSPECIFIED | PRIVATE | PUBLIC | SECRET
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable FCMNotificationVisibility

data FCMShowNotification = SHOW | DO_NOT_SHOW
  deriving (Show, Eq, Read, Generic)
  deriving (PrettyShow) via Showable FCMShowNotification

instance ToJSON FCMShowNotification where
  toJSON SHOW = "true"
  toJSON _ = "false"

instance FromJSON FCMShowNotification where
  parseJSON = withText "FCMShowNotification" \case
    "true" -> pure SHOW
    "false" -> pure DO_NOT_SHOW
    str -> typeMismatch "FCMShowNotification" (String str)

-- | HTTP request headers
type FCMHeaders = Map Text Text

-- | Target to send a message to. Target can be only one of the following:
-- data FCMTarget = FCMTopic Text | FCMToken Text | FCMCondition Text

-- | Represents a color in the RGBA color space
data FCMColor = FCMColor
  { fcmRed :: Int,
    fcmGreen :: Int,
    fcmBlue :: Int,
    fcmAlpha :: Int
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMColor)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMColor)

-- | Options for features provided by the FCM SDK for Android.
newtype FCMAndroidOptions = FCMAndroidOptions
  { fcmdAnalyticsLabel :: Maybe Text
  }
  deriving (Eq, Show)
  deriving newtype (PrettyShow)

$(makeLenses ''FCMAndroidOptions)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMAndroidOptions)

instance Default FCMAndroidOptions where
  def = FCMAndroidOptions Nothing

-- | Options for features provided by the FCM SDK for iOS
data FCMApnsOptions = FCMApnsOptions
  { fcmaAnalyticsLabel :: !(Maybe Text),
    fcmaImage :: !(Maybe FCMNotificationIconUrl)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMApnsOptions)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMApnsOptions)

instance Default FCMApnsOptions where
  def = FCMApnsOptions Nothing Nothing

-- | Options for features provided by the FCM SDK for iOS
data FCMWebpushOptions = FCMWebpushOptions
  { fcmwAnalyticsLabel :: !(Maybe Text),
    fcmwLink :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMWebpushOptions)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMWebpushOptions)

instance Default FCMWebpushOptions where
  def = FCMWebpushOptions Nothing Nothing

-- | Settings to control notification LED
data FCMLightSettings = FCMLightSettings
  { fcmLightOnDuration :: !(Maybe Text),
    fcmLightOffDuration :: !(Maybe Text),
    fcmColor :: !(Maybe FCMColor)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMLightSettings)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMLightSettings)

instance Default FCMLightSettings where
  def = FCMLightSettings Nothing Nothing Nothing

-- | Basic notification template to use across all platforms
data FCMNotification = FCMNotification
  { fcmTitle :: !(Maybe FCMNotificationTitle),
    fcmBody :: !(Maybe FCMNotificationBody),
    fcmImage :: !(Maybe FCMNotificationIconUrl)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMNotification)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMNotification)

instance Default FCMNotification where
  def = FCMNotification Nothing Nothing Nothing

data FCMOverlayReq = FCMOverlayReq
  { title :: Maybe Text,
    description :: Maybe Text,
    imageUrl :: Maybe Text,
    okButtonText :: Maybe Text,
    cancelButtonText :: Maybe Text,
    actions :: [Text],
    actions2 :: [FCMActions],
    secondaryActions2 :: Maybe [FCMActions],
    link :: Maybe Text,
    endPoint :: Maybe Text,
    method :: Maybe Text,
    reqBody :: Value,
    delay :: Maybe Int,
    contactSupportNumber :: Maybe Text,
    toastMessage :: Maybe Text,
    secondaryActions :: Maybe [Text],
    socialMediaLinks :: Maybe [FCMMediaLink],
    showPushNotification :: Maybe Bool
  }
  deriving (Eq, Show, Generic, ToSchema, FromJSON, PrettyShow)

data FCMActions = FCMActions
  { primaryAction :: FCMOverlayAction,
    dependentActions :: [FCMActions]
  }
  deriving (Eq, Show, Generic, ToSchema, FromJSON, PrettyShow, ToJSON)

data FCMOverlayAction = CALL_API CallAPIDetails | SET_DRIVER_ONLINE | OPEN_LINK OpenLinkDetails | CALL_SUPPORT CallSupportDetails | OPEN_SUBSCRIPTION | NAVIGATE NavigationDetails
  deriving (Eq, Show, Generic, ToSchema, Read)
  deriving (PrettyShow) via Showable FCMOverlayAction

data CallAPIDetails = CallAPIDetails
  { endPoint :: Text,
    method :: Text,
    reqBody :: Value
  }
  deriving (Eq, Show, Generic, PrettyShow, ToSchema, FromJSON, ToJSON, Read)

data NavigationDetails = NavigationDetails
  { lat :: Double,
    long :: Double
  }
  deriving (Eq, Show, Generic, PrettyShow, ToSchema, FromJSON, ToJSON, Read)

newtype OpenLinkDetails = OpenLinkDetails
  { link :: Text
  }
  deriving (Eq, Show, Generic)
  deriving newtype (PrettyShow, ToSchema, FromJSON, ToJSON, Read)

newtype CallSupportDetails = CallSupportDetails
  { contactSupportNumber :: Text
  }
  deriving (Eq, Show, Generic)
  deriving newtype (PrettyShow, ToSchema, FromJSON, ToJSON, Read)

instance ToJSON FCMOverlayAction where
  toJSON (CALL_API details) =
    object
      [ "actionName" .= String "CALL_API",
        "actionDetails" .= toJSON details
      ]
  toJSON SET_DRIVER_ONLINE =
    object
      [ "actionName" .= String "SET_DRIVER_ONLINE",
        "actionDetails" .= Null
      ]
  toJSON (OPEN_LINK details) =
    object
      [ "actionName" .= String "OPEN_LINK",
        "actionDetails" .= toJSON details
      ]
  toJSON (CALL_SUPPORT details) =
    object
      [ "actionName" .= String "CALL_SUPPORT",
        "actionDetails" .= toJSON details
      ]
  toJSON OPEN_SUBSCRIPTION =
    object
      [ "actionName" .= String "OPEN_SUBSCRIPTION",
        "actionDetails" .= Null
      ]
  toJSON (NAVIGATE details) =
    object
      [ "actionName" .= String "NAVIGATE",
        "actionDetails" .= toJSON details
      ]

instance FromJSON FCMOverlayAction where
  parseJSON = withObject "FCMOverlayAction" $ \o -> do
    actionName <- o .: "actionName" :: Parser Text
    actionDetails <- o .:? "actionDetails" :: Parser (Maybe Value)
    case (actionName, actionDetails) of
      ("CALL_API", Just details) -> CALL_API <$> parseJSON details
      ("SET_DRIVER_ONLINE", _) -> return SET_DRIVER_ONLINE
      ("OPEN_LINK", Just details) -> OPEN_LINK <$> parseJSON details
      ("CALL_SUPPORT", Just details) -> CALL_SUPPORT <$> parseJSON details
      ("OPEN_SUBSCRIPTION", _) -> return OPEN_SUBSCRIPTION
      ("NAVIGATE", Just details) -> NAVIGATE <$> parseJSON details
      _ -> fail "Invalid JSON format for FCMOverlayAction"

data FCMMediaLink = FCMMediaLink
  { prefixImage :: Maybe Text,
    suffixImage :: Maybe Text,
    link :: Text,
    linkText :: Maybe Text,
    height :: Maybe Text,
    width :: Maybe Text
  }
  deriving (Eq, Show, Generic, PrettyShow, ToSchema, FromJSON)

$(makeLenses ''FCMMediaLink)

instance ToJSON FCMMediaLink where
  toJSON = genericToJSON removeNullFields

-- | Notification to send to android devices for overlays
data FCMOverlayNotificationJSON = FCMOverlayNotificationJSON
  { title :: !(Maybe Text),
    description :: !(Maybe Text), -- need confirmation from UI if mandatory
    imageUrl :: !(Maybe Text),
    okButtonText :: !(Maybe Text),
    cancelButtonText :: !(Maybe Text),
    actions :: ![Text],
    link :: !(Maybe Text),
    method :: Maybe Text,
    reqBody :: Value,
    actions2 :: ![FCMActions],
    secondaryActions2 :: !(Maybe [FCMActions]),
    endPoint :: Maybe Text,
    titleVisibility :: !Bool,
    descriptionVisibility :: !Bool,
    buttonOkVisibility :: !Bool,
    buttonCancelVisibility :: !Bool,
    buttonLayoutVisibility :: !Bool,
    imageVisibility :: !Bool,
    delay :: !(Maybe Int),
    contactSupportNumber :: !(Maybe Text),
    toastMessage :: !(Maybe Text),
    secondaryActions :: !(Maybe [Text]),
    socialMediaLinks :: !(Maybe [FCMMediaLink]),
    showPushNotification :: !(Maybe Bool)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMOverlayNotificationJSON)

instance ToJSON FCMOverlayNotificationJSON where
  toJSON = genericToJSON removeNullFields

instance FromJSON FCMOverlayNotificationJSON where
  parseJSON = genericParseJSON removeNullFields

-- | Notification to send to android devices
data FCMAndroidNotification = FCMAndroidNotification
  { fcmdTitle :: !(Maybe FCMNotificationTitle),
    fcmdBody :: !(Maybe FCMNotificationBody),
    fcmdIcon :: !(Maybe FCMNotificationIconUrl),
    fcmdColor :: !(Maybe Text),
    fcmdSound :: !(Maybe Text),
    fcmdTag :: !(Maybe FCMNotificationType),
    fcmdClickAction :: !(Maybe Text),
    fcmdBodyLocKey :: !(Maybe Text),
    fcmdBodyLockArgs :: !(Maybe [Text]),
    fcmdTitleLocKey :: !(Maybe Text),
    fcmdTitleLockArgs :: !(Maybe [Text]),
    fcmdChannelId :: !(Maybe Text),
    fcmdTicker :: !(Maybe Text),
    fcmdSticky :: !(Maybe Bool),
    fcmdEventTime :: !(Maybe Text),
    fcmdLocalOnly :: !(Maybe Bool),
    fcmdNotificationPriority :: !(Maybe FCMNotificationPriority),
    fcmdDefaultSound :: !(Maybe Bool),
    fcmdDefalutVibrateTimings :: !(Maybe Bool),
    fcmdDefaultLightSettings :: !(Maybe Bool),
    fcmdVibrateTimings :: !(Maybe [Text])
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMAndroidNotification)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMAndroidNotification)

instance Default FCMAndroidNotification where
  def =
    let sound = Just "default"
        channelId = Just "General"
     in FCMAndroidNotification
          { fcmdTitle = Nothing,
            fcmdBody = Nothing,
            fcmdIcon = Nothing,
            fcmdColor = Nothing,
            fcmdSound = sound,
            fcmdTag = Nothing,
            fcmdClickAction = Nothing,
            fcmdBodyLocKey = Nothing,
            fcmdBodyLockArgs = Nothing,
            fcmdTitleLocKey = Nothing,
            fcmdTitleLockArgs = Nothing,
            fcmdChannelId = channelId,
            fcmdTicker = Nothing,
            fcmdSticky = Nothing,
            fcmdEventTime = Nothing,
            fcmdLocalOnly = Nothing,
            fcmdNotificationPriority = Nothing,
            fcmdDefaultSound = Nothing,
            fcmdDefalutVibrateTimings = Nothing,
            fcmdDefaultLightSettings = Nothing,
            fcmdVibrateTimings = Nothing
          }

-- | FCM payload
data FCMData a = FCMData
  { fcmNotificationType :: FCMNotificationType,
    fcmShowNotification :: FCMShowNotification,
    fcmEntityType :: FCMEntityType,
    fcmEntityIds :: Text,
    fcmEntityData :: a,
    fcmNotificationJSON :: FCMAndroidNotification,
    fcmNotificationId :: Maybe Text,
    fcmOverlayNotificationJSON :: Maybe FCMOverlayNotificationJSON
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMData)

instance (ToJSON a) => ToJSON (FCMData a) where
  toJSON FCMData {..} =
    object
      [ "notification_type" .= fcmNotificationType,
        "show_notification" .= fcmShowNotification,
        "entity_type" .= fcmEntityType,
        "entity_ids" .= fcmEntityIds,
        "entity_data" .= encodeToText fcmEntityData,
        "notification_json" .= encodeToText fcmNotificationJSON,
        "notification_id" .= fcmNotificationId,
        "driver_notification_payload" .= (encodeToText <$> fcmOverlayNotificationJSON)
      ]

instance (FromJSON a) => FromJSON (FCMData a) where
  parseJSON = withObject "FCMData" \o ->
    FCMData
      <$> o .: "notification_type"
      <*> o .: "show_notification"
      <*> o .: "entity_type"
      <*> o .: "entity_ids"
      <*> (o .: "entity_data" >>= parseNotificationJson)
      <*> (o .: "notification_json" >>= parseNotificationJson)
      <*> o .:? "notification_id"
      <*> (o .:? "driver_notification_payload" >>= maybe (pure Nothing) parseNotificationJson)
    where
      parseNotificationJson str =
        maybe (typeMismatch "Json string" (String str)) pure $ decodeFromText str

-- | Android specific options for messages sent through FCM connection server
data FCMAndroidConfig a = FCMAndroidConfig
  { fcmdCollapseKey :: !(Maybe Text),
    fcmdPriority :: !(Maybe FCMAndroidMessagePriority),
    fcmdTtl :: !(Maybe Text),
    fcmdRestrictedPackageName :: !(Maybe Text),
    fcmdData :: !(Maybe (FCMData a)),
    fcmdOptions :: !(Maybe FCMAndroidOptions),
    fcmdDirectBootOk :: !(Maybe Bool)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMAndroidConfig)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMAndroidConfig)

instance Default (FCMAndroidConfig a) where
  def =
    let z = Nothing
     in FCMAndroidConfig z z z z z z z

-- | Apple Push Notification Service specific options
data FCMAlert = FCMAlert
  { fcmTitle :: !(Maybe Text),
    fcmBody :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMAlert)

instance ToJSON FCMAlert where
  toJSON FCMAlert {..} =
    object
      [ "title" .= fcmTitle,
        "body" .= fcmBody
      ]

instance FromJSON FCMAlert where
  parseJSON = withObject "FCMAlert" \o ->
    FCMAlert
      <$> o .: "title"
      <*> o .: "body"

instance Default FCMAlert where
  def = FCMAlert Nothing Nothing

-----------------------------------------

data FCMaps a = FCMaps
  { fcmAlert :: !(Maybe FCMAlert),
    fcmData :: !(Maybe (FCMData a)),
    fcmCategory :: !(Maybe FCMNotificationType),
    fcmMutableContent :: !Int,
    fcmSound :: !(Maybe Text),
    fcmContentAvailable :: !Int,
    fcmBadge :: !(Maybe Int)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMaps)

instance (ToJSON a) => ToJSON (FCMaps a) where
  toJSON FCMaps {..} =
    object $
      catMaybes
        [ ("alert" .=) <$> fcmAlert,
          ("data" .=) <$> fcmData,
          ("category" .=) <$> fcmCategory,
          ("sound" .=) <$> fcmSound,
          ("badge" .=) <$> fcmBadge
        ]
        ++ [ "mutable-content" .= fcmMutableContent,
             "content-available" .= fcmContentAvailable
           ]

instance (FromJSON a) => FromJSON (FCMaps a) where
  parseJSON = withObject "FCMaps" \o ->
    FCMaps
      <$> o .: "alert"
      <*> o .: "data"
      <*> o .: "category"
      <*> o .: "mutable-content"
      <*> o .: "sound"
      <*> o .: "content-available"
      <*> o .: "badge"

instance Default (FCMaps a) where
  def = FCMaps Nothing Nothing Nothing 1 Nothing 1 Nothing

newtype FCMApnPayload a = FCMApnPayload
  { fcmAps :: Maybe (FCMaps a)
  }
  deriving (Eq, Show)
  deriving newtype (PrettyShow)

$(makeLenses ''FCMApnPayload)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMApnPayload)

instance Default (FCMApnPayload a) where
  def = FCMApnPayload Nothing

newtype FCMApnHeaders = FCMApnHeaders
  { fcmApnsPriority :: Maybe Text
  }
  deriving (Eq, Show)
  deriving newtype (PrettyShow)

$(makeLenses ''FCMApnHeaders)

instance ToJSON FCMApnHeaders where
  toJSON FCMApnHeaders {..} =
    object
      [ "apns-priority" .= fcmApnsPriority
      ]

instance FromJSON FCMApnHeaders where
  parseJSON = withObject "FCMApnHeaders" \o ->
    FCMApnHeaders
      <$> o .: "apns-priority"

instance Default FCMApnHeaders where
  def = FCMApnHeaders Nothing

data FCMApnsConfig a = FCMApnsConfig
  { fcmaHeaders :: !(Maybe FCMApnHeaders),
    fcmaPayload :: !(Maybe (FCMApnPayload a)),
    fcmaOptions :: !(Maybe FCMApnsOptions)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMApnsConfig)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMApnsConfig)

instance Default (FCMApnsConfig a) where
  def = FCMApnsConfig Nothing Nothing Nothing

-- | Webpush protocol specific options
data FCMWebpushConfig a = FCMWebpushConfig
  { fcmwHeaders :: !(Maybe FCMHeaders),
    fcmwData :: !(Maybe (FCMData a)),
    fcmwNotification :: !(Maybe Value),
    fcmwOptions :: !(Maybe FCMWebpushOptions)
  }
  deriving (Eq, Show, Generic)
  deriving (PrettyShow) via Showable (FCMWebpushConfig a)

$(makeLenses ''FCMWebpushConfig)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMWebpushConfig)

instance Default (FCMWebpushConfig a) where
  def = FCMWebpushConfig Nothing Nothing Nothing Nothing

-- | Message to send by Firebase Cloud Messaging Service
data FCMMessage a b = FCMMessage
  { fcmToken :: !(Maybe FCMRecipientToken),
    fcmTopic :: !(Maybe Text),
    fcmCondition :: !(Maybe Text),
    fcmNotification :: !(Maybe FCMNotification),
    fcmAndroid :: !(Maybe (FCMAndroidConfig a)),
    fcmWebpush :: !(Maybe (FCMWebpushConfig a)),
    fcmApns :: !(Maybe (FCMApnsConfig b)),
    fcmOptions :: !(Maybe FCMAndroidOptions)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMMessage)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMMessage)

instance Default (FCMMessage a b) where
  def =
    let z = Nothing
     in FCMMessage z z z z z z z z

newtype FCMRequest a b = FCMRequest
  { fcmeMessage :: FCMMessage a b
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (PrettyShow)

$(makeLenses ''FCMRequest)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMRequest)

-- | Priority levels of a notification
data FCMErrorCode
  = -- No more information is available about this error
    UNSPECIFIED_ERROR
  | -- (HTTP error code = 400) Request parameters were invalid.
    -- An extension of type google.rpc.BadRequest is returned to specify
    -- which field was invalid
    INVALID_ARGUMENT
  | -- (HTTP error code = 404) App instance was unregistered from FCM. This usually means that the token used is no longer valid and a new one must be used
    UNREGISTERED
  | -- (HTTP error code = 403) The authenticated sender Id is different from the sender Id for the registration token
    SENDER_ID_MISMATCH
  | -- (HTTP error code = 429) Sending limit exceeded for the message target. An extension of type google.rpc.QuotaFailure is returned to specify which quota got exceeded
    QUOTA_EXCEEDED
  | -- (HTTP error code = 401) APNs certificate or auth key was invalid or missing. Deprecated. Use THIRD_PARTY_AUTH_ERROR
    APNS_AUTH_ERROR
  | -- (HTTP error code = 503) The server is overloaded
    UNAVAILABLE
  | -- (HTTP error code = 500) An unknown internal error occurred
    INTERNAL
  | -- (HTTP error code = 401) APNs certificate or web push auth key was invalid or missing
    THIRD_PARTY_AUTH_ERROR
  | PERMISSION_DENIED
  | UNAUTHENTICATED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable FCMErrorCode

data FCMError = FCMError
  { fcmerrCode :: Int,
    fcmerrStatus :: FCMErrorCode,
    fcmerrMessage :: !Text
  }
  deriving (Show, Eq, Read, Generic, PrettyShow)

$(makeLenses ''FCMError)

$(deriveJSON (aesonPrefix snakeCase) ''FCMError)

-- | Message to send by Firebase Cloud Messaging Service
data FCMResponse = FCMResponse
  { fcmName :: Maybe Text,
    fcmerrError :: Maybe FCMError
  }
  deriving (Show, Eq, Read, Generic, PrettyShow)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMResponse)

$(mkBeamInstancesForEnum ''FCMOverlayAction)

data LiveActivityReq = LiveActivityReq
  { liveActivityToken :: Text, -- live activity token
    liveActivityReqType :: Text, -- request to be update, end, start
    liveActivityNotificationType :: Text, -- notification type , SEARCH_CANCELLED, RIDE_CANCELLED
    liveActivityContentState :: LiveActivityContentState, -- live activity content state
    liveActivityApnsPriority :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ApnsAPIRequest = ApnsAPIRequest
  { message :: Message
  }
  deriving (Generic, Eq, Show)

instance ToJSON ApnsAPIRequest where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}

instance FromJSON ApnsAPIRequest where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data Message = Message
  { token :: FCMRecipientToken,
    apns :: Apns
  }
  deriving (Generic, Eq, Show)

instance ToJSON Message where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}

instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data Apns = Apns
  { live_activity_token :: Text,
    headers :: ApnsHeaders,
    payload :: Payload
  }
  deriving (Generic, Eq, Show)

instance ToJSON Apns where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}

instance FromJSON Apns where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data ApnsHeaders = ApnsHeaders
  { apns_priority :: Text
  }
  deriving (Generic, Eq, Show)

jsonApnsHeadersData :: Options
jsonApnsHeadersData =
  defaultOptions
    { fieldLabelModifier = \case
        "apns_priority" -> "apns-priority"
        other -> other
    }

instance ToJSON ApnsHeaders where
  toJSON = genericToJSON jsonApnsHeadersData

instance FromJSON ApnsHeaders where
  parseJSON = genericParseJSON jsonApnsHeadersData

data Payload = Payload
  { aps :: Aps
  }
  deriving (Generic, Eq, Show)

instance ToJSON Payload where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}

instance FromJSON Payload where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data Aps = Aps
  { timestamp :: Int,
    alert :: Maybe Alert,
    dismissal_date :: Maybe Int,
    content_available :: Int,
    event :: Text,
    content_state :: LiveActivityContentState
  }
  deriving (Generic, Eq, Show)

instance ToJSON Aps where
  toJSON = genericToJSON jsonApsData

instance FromJSON Aps where
  parseJSON = genericParseJSON jsonApsData

jsonApsData :: Options
jsonApsData =
  defaultOptions
    { fieldLabelModifier = \case
        "content_available" -> "content-available"
        "content_state" -> "content-state"
        "dismissal_date" -> "dismissal-date"
        other -> other
    }

data Alert = Alert
  { title :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Alert where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}

instance FromJSON Alert where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data LiveActivityContentState = LiveActivityContentState
  { driverInfo :: Maybe DriverInfo,
    bookingInfo :: Maybe BookingInfo,
    activityStatus :: Text, -- searching , arriving , waiting , onRide , rideCompleted , reallocated
    timerDuration :: Maybe Common.BatchConfig,
    customMessage :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data DriverInfo = DriverInfo
  { rideOtp :: Maybe Text,
    driverName :: Maybe Text,
    distanceLeft :: Maybe Text,
    totalDistance :: Maybe Text,
    driverNumber :: Maybe Text,
    driverProfile :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data BookingInfo = BookingInfo
  { vehicleName :: Maybe Text,
    vehicleNumber :: Maybe Text,
    vehicleVariant :: Maybe Text,
    vehicleColor :: Maybe Text,
    source :: Maybe Text,
    destination :: Maybe Text,
    estimatedFare :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
