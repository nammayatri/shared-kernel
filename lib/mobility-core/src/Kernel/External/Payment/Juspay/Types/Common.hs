{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Payment.Juspay.Types.Common where

import Data.Aeson.Types
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.Common (HighPrecMoney)

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
  | MANDATE_PAUSED
  | MANDATE_EXPIRED
  | NOTIFICATION_FAILED
  | NOTIFICATION_SUCCEEDED
  | ORDER_AUTHORIZED
  | TXN_CHARGED
  | TXN_FAILED
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Currency = INR
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (ToSchema)

derivePersistField "Currency"

$(mkBeamInstancesForEnum ''Currency)

-- Generic instances for type with single value will not work
instance FromJSON Currency where
  parseJSON (String "INR") = pure INR
  parseJSON (String _) = parseFail "Expected \"INR\""
  parseJSON e = typeMismatch "String" e

instance ToJSON Currency where
  toJSON = String . show

data MandateType = OPTIONAL | REQUIRED
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

derivePersistField "MandateType"

data MandateFrequency = ONETIME | DAILY | WEEKLY | FORTNIGHTLY | MONTHLY | BIMONTHLY | QUARTERLY | HALFYEARLY | YEARLY | ASPRESENTED
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

derivePersistField "MandateFrequency"

data MandateStatus = CREATED | ACTIVE | FAILURE | PAUSED | EXPIRED | REVOKED
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

derivePersistField "MandateStatus"

data TransactionStatus
  = NEW
  | PENDING_VBV
  | CHARGED
  | AUTHENTICATION_FAILED
  | AUTHORIZATION_FAILED
  | JUSPAY_DECLINED
  | AUTHORIZING
  | COD_INITIATED
  | STARTED
  | AUTO_REFUNDED
  | CLIENT_AUTH_TOKEN_EXPIRED -- Domain status, not part of Juspay Euler status types
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

derivePersistField "TransactionStatus"

$(mkBeamInstancesForEnum ''TransactionStatus)

type OrderStatusResp = OrderData

data OrderData = OrderData
  { order_id :: Text,
    txn_uuid :: Maybe Text,
    status_id :: Int,
    event_name :: Maybe PaymentStatus, --- only webhook res contains this field ---
    status :: TransactionStatus,
    payment_method_type :: Maybe Text,
    payment_method :: Maybe Text,
    resp_message :: Maybe Text,
    resp_code :: Maybe Text,
    gateway_reference_id :: Maybe Text,
    amount :: Double,
    currency :: Currency,
    date_created :: Maybe UTCTime,
    mandate :: Maybe MandateData,
    payer_vpa :: Maybe Text,
    upi :: Maybe Upi
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data MandateData = MandateData
  { mandate_status :: MandateStatus,
    start_date :: Text,
    end_date :: Text,
    mandate_id :: Text,
    frequency :: MandateFrequency,
    max_amount :: HighPrecMoney
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Upi = Upi
  { payer_app :: Maybe Text,
    payer_app_name :: Maybe Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
