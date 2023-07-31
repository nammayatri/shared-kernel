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
import Kernel.Prelude
  ( Applicative (pure),
    Double,
    Eq,
    Generic,
    HasField (hasField),
    Int,
    Maybe,
    Read,
    Show,
    Text,
    ToSchema,
    UTCTime,
    show,
    (.),
  )
import Kernel.Storage.Esqueleto (derivePersistField)

data Currency = INR
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

derivePersistField "Currency"

-- Generic instances for type with single value will not work
instance FromJSON Currency where
  parseJSON (String "INR") = pure INR
  parseJSON (String _) = parseFail "Expected \"INR\""
  parseJSON e = typeMismatch "String" e

instance ToJSON Currency where
  toJSON = String . show

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
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

derivePersistField "TransactionStatus"

-- order status api return the same data as webhook
type OrderStatusResp = OrderData

data OrderData = OrderData
  { order_id :: Text,
    txn_uuid :: Maybe Text,
    status_id :: Int,
    status :: TransactionStatus,
    payment_method_type :: Maybe Text,
    payment_method :: Maybe Text,
    resp_message :: Maybe Text,
    resp_code :: Maybe Text,
    gateway_reference_id :: Maybe Text,
    amount :: Double,
    currency :: Currency,
    date_created :: Maybe UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
