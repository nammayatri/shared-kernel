{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Notification.Interface.Types where

import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.External.Notification.FCM.Types
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Notification.GRPC.Types as GRPC
import qualified Kernel.External.Notification.PayTM.Types as PayTM
import qualified Kernel.External.Notification.Types as Interface
import Kernel.Prelude

data NotificationServiceConfig = FCMConfig FCM.FCMConfig | PayTMConfig PayTM.PayTMConfig | GRPCConfig GRPC.GRPCConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Category
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
  | DRIVER_UNSUBSCRIBED
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
  | DRIVER_REQUEST_REJECTED
  | WMB_TRIP_ASSIGNED
  | WMB_TRIP_STARTED
  | WMB_TRIP_FINISHED
  | FLEET_CONSENT
  | OPERATOR_CONSENT
  | DRIVER_UNLINK_FROM_FLEET
  | DRIVER_UNLINK_FROM_OPERATOR
  | FLEET_UNLINK_FROM_OPERATOR
  | SAFETY_ALERT_RIDE_STOPPAGE
  | EKD_LIVE_CALL_FEEDBACK
  | DRUNK_AND_DRIVE_VIOLATION_WARNING
  | ACCOUNT_DELETED
  | PICKUP_INSTRUCTIONS
  | PREPAID_RECHARGE_SUCCESS
  | PREPAID_BALANCE_UPDATE
  | PAYOUT_INITIATED
  | PAYOUT_FAILED
  | PAYOUT_COMPLETED
  | EXOTEL_CALL_UNREACHABLE
  | END_RIDE_OFFERS
  deriving (Show, Eq, Read, Generic, Ord, ToSchema, ToJSON, FromJSON)

$(mkBeamInstancesForEnum ''Category)

data SubCategory
  = ByUser
  | ByMerchant
  | ByDriver
  | ByAllocator
  | ByApplication
  deriving (Show, Eq, Read, Generic, Ord, ToSchema, ToJSON, FromJSON)

$(mkBeamInstancesForEnum ''SubCategory)

data ShowNotification = SHOW | DO_NOT_SHOW
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data EntityType = SearchRequest | Product | Merchant | Person | EditLocation
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data Auth = Auth
  { recipientId :: Text,
    fcmToken :: Maybe Text,
    notificationToken :: Maybe Text
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data MessagePriority = NORMAL | HIGH
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data Entity a = Entity
  { entityType :: EntityType,
    entityIds :: Text,
    entityData :: a
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data NotificationReq a b = NotificationReq
  { auth :: Auth,
    category :: Category,
    subCategory :: Maybe SubCategory,
    showNotification :: ShowNotification,
    messagePriority :: Maybe MessagePriority,
    entity :: Entity a,
    dynamicParams :: b,
    body :: Text,
    title :: Text,
    ttl :: Maybe UTCTime,
    sound :: Maybe Text
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data NotficationServiceHandler m a b = NotficationServiceHandler
  { getNotificationServiceList :: m [Interface.NotificationService],
    getServiceConfig :: Interface.NotificationService -> m NotificationServiceConfig,
    iosModifier :: FCMData a -> FCMData b
  }
