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
import Kernel.Prelude
import Kernel.Utils.Common

data PaymentStatus
  = ORDER_SUCCEEDED
  | ORDER_REFUNDED
  | ORDER_FAILED
  | ORDER_REFUND_FAILED
  | TXN_CREATED
  | REFUND_MANUAL_REVIEW_NEEDED
  | REFUND_INITIATED
  | AUTO_REFUND_SUCCEEDED
  | AUTO_REFUND_FAILED
  | MANDATE_CREATED
  | MANDATE_ACTIVATED
  | MANDATE_FAILED
  | MANDATE_REVOKED
  | NOTIFICATION_FAILED
  | NOTIFICATION_SUCCEEDED
  | ORDER_AUTHORIZED
  | TXN_CHARGED
  | TXN_FAILED
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WebhookReq = WebhookReq
  { id :: Text,
    date_created :: UTCTime,
    event_name :: PaymentStatus,
    content :: OrderStatusContent
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

type WebhookOrderData = OrderData

data OrderStatusContent = OrderStatusContent
  { order :: Maybe WebhookOrderData,
    mandate :: Maybe WebhookMandateData
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WebhookMandateData = WebhookMandateData
  { status :: MandateStatus,
    start_date :: Text,
    end_date :: Text,
    mandate_id :: Text,
    frequency :: MandateFrequency,
    max_amount :: HighPrecMoney
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
