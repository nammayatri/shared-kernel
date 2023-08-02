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

module Kernel.External.Notification.Interface.Types where

import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Notification.PayTM.Types as PayTM
import Kernel.Prelude

data NotificationServiceConfig = FCMConfig FCM.FCMConfig | PayTMConfig PayTM.PayTMConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Category
  = REGISTRATION_APPROVED
  | EXPIRED_CASE
  | CANCELLED_PRODUCT
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
  | CLEARED_FARE
  | CANCELLED_SEARCH_REQUEST
  | NEW_MESSAGE
  | REFERRAL_ACTIVATED
  | CHAT_MESSAGE
  | SAFETY_ALERT
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data SubCategory
  = ByUser
  | ByMerchant
  | ByDriver
  | ByAllocator
  | ByApplication
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data ShowNotification = SHOW | DO_NOT_SHOW
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data EntityType = SearchRequest | Product | Merchant | Person
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
    title :: Text
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
