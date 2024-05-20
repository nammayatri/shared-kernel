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
  deriving stock (Show, Eq, Generic, Read)
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
  deriving stock (Show, Read, Eq, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

derivePersistField "MandateType"

$(mkBeamInstancesForEnum ''MandateType)

data MandateFrequency = ONETIME | DAILY | WEEKLY | FORTNIGHTLY | MONTHLY | BIMONTHLY | QUARTERLY | HALFYEARLY | YEARLY | ASPRESENTED
  deriving stock (Show, Read, Eq, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''MandateFrequency)

derivePersistField "MandateFrequency"

data MandateStatus = CREATED | ACTIVE | FAILURE | PAUSED | EXPIRED | REVOKED
  deriving stock (Show, Read, Eq, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''MandateStatus)

derivePersistField "MandateStatus"

data NotificationStatus = NOTIFICATION_CREATED | NOTIFICATION_FAILURE | PENDING | SUCCESS
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

derivePersistField "NotificationStatus"

instance FromJSON NotificationStatus where
  parseJSON (String "CREATED") = pure NOTIFICATION_CREATED
  parseJSON (String "FAILURE") = pure NOTIFICATION_FAILURE
  parseJSON (String "PENDING") = pure PENDING
  parseJSON (String "SUCCESS") = pure SUCCESS
  parseJSON (String _) = parseFail "Expected type"
  parseJSON e = typeMismatch "String" e

instance ToJSON NotificationStatus where
  toJSON NOTIFICATION_CREATED = "CREATED"
  toJSON NOTIFICATION_FAILURE = "FAILURE"
  toJSON PENDING = "PENDING"
  toJSON SUCCESS = "SUCCESS"

data RefundStatus = REFUND_PENDING | REFUND_FAILURE | REFUND_SUCCESS | MANUAL_REVIEW
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (ToSchema)

derivePersistField "RefundStatus"

$(mkBeamInstancesForEnum ''RefundStatus)

instance FromJSON RefundStatus where
  parseJSON (String "FAILURE") = pure REFUND_FAILURE
  parseJSON (String "PENDING") = pure REFUND_PENDING
  parseJSON (String "SUCCESS") = pure REFUND_SUCCESS
  parseJSON (String "MANUAL_REVIEW") = pure MANUAL_REVIEW
  parseJSON (String _) = parseFail "Expected type"
  parseJSON e = typeMismatch "String" e

instance ToJSON RefundStatus where
  toJSON REFUND_FAILURE = "FAILURE"
  toJSON REFUND_PENDING = "PENDING"
  toJSON REFUND_SUCCESS = "SUCCESS"
  toJSON MANUAL_REVIEW = "MANUAL_REVIEW"

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
    txn_id :: Maybe Text,
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
    bank_error_code :: Maybe Text,
    bank_error_message :: Maybe Text,
    upi :: Maybe Upi,
    metadata :: Maybe MetaData,
    additional_info :: Maybe AdditionalInfo,
    links :: Maybe LinkData,
    amount_refunded :: Maybe Double,
    refunds :: Maybe [RefundsData]
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
    payer_app_name :: Maybe Text,
    payer_vpa :: Maybe Text,
    txn_flow_type :: Maybe Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data MetaData = MetaData
  { is_retried_order :: Maybe Text,
    juspay_internal_retarget_configs :: Maybe RetargetConfigs
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data RetargetConfigs = RetargetConfigs
  { retarget_payment_link_expiry :: Maybe Text,
    is_retargeted_order :: Maybe Text,
    retarget_done_count :: Maybe Text,
    max_retarget_limit :: Maybe Text,
    retarget_payment_link :: Maybe Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype LinkData = LinkData
  { retarget_payment_links :: Maybe RetargetConfigs
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AdditionalInfo = AdditionalInfo
  { retarget_payment_info :: Maybe RetargetConfigs,
    mandate_retry_info :: Maybe MandateRetryInfo,
    is_business_retried :: Maybe Text,
    is_technical_retried :: Maybe Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data MandateRetryInfo = MandateRetryInfo
  { is_retried_order :: Maybe Text,
    retries_done_count :: Maybe Text,
    is_technical_retried :: Maybe Text,
    allowed_retry :: Maybe Text,
    is_business_retried :: Maybe Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data RefundsData = RefundsData
  { id :: Text,
    amount :: Double,
    status :: RefundStatus,
    error_message :: Maybe Text,
    error_code :: Maybe Text,
    initiated_by :: Maybe Text,
    unique_request_id :: Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
