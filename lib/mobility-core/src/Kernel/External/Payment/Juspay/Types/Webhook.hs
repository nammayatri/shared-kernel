{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kernel.External.Payment.Juspay.Types.Webhook where

import Data.Aeson
import Kernel.External.Payment.Juspay.Types.Common
import Kernel.External.Payment.Juspay.Types.Mandate (SourceInfo)
import Kernel.Prelude
import Kernel.Utils.Common

data WebhookReq = WebhookReq
  { id :: Text,
    date_created :: UTCTime,
    event_name :: PaymentStatus,
    content :: OrderAndNotificationStatusContent
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

type WebhookOrderData = OrderData

data OrderAndNotificationStatusContent = OrderAndNotificationStatusContent
  { order :: Maybe WebhookOrderData,
    mandate :: Maybe WebhookMandateData,
    notification :: Maybe WebhookNotificationData,
    txn :: Maybe WebhookTxnData
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WebhookMandateData = WebhookMandateData
  { status :: MandateStatus,
    start_date :: Text,
    end_date :: Text,
    mandate_id :: Text,
    frequency :: MandateFrequency,
    max_amount :: HighPrecMoney,
    order_id :: Text,
    payment_info :: Maybe PaymentInfo
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WebhookNotificationData = WebhookNotificationData
  { status :: NotificationStatus,
    source_object :: Maybe Text,
    source_info :: Maybe SourceInfo,
    notification_type :: Maybe Text,
    object_reference_id :: Text,
    id :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype PaymentInfo = PaymentInfo {upi :: Maybe Upi}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WebhookTxnData = WebhookTxnData
  { txn_id :: Text,
    txn_uuid :: Maybe Text,
    status :: TransactionStatus,
    order_id :: Text,
    error_code :: Maybe Text,
    error_message :: Maybe Text,
    txn_amount :: Double,
    status_id :: Int,
    currency :: Currency
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
