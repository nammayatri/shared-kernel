{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.Interface.Types
  ( PaymentSettlementReport (..),
    TxnType (..),
    TxnStatus (..),
    SettlementType (..),
    SettlementMode (..),
    PaymentMethodType (..),
    DisputeType (..),
    ParseSettlementResult (..),
  )
where

import Data.Aeson (Value)
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)

data PaymentSettlementReport = PaymentSettlementReport
  { orderId :: Text,
    txnId :: Maybe Text,
    rrn :: Maybe Text,
    utr :: Maybe Text,
    txnType :: TxnType,
    txnStatus :: TxnStatus,
    txnDate :: Maybe UTCTime,
    txnAmount :: HighPrecMoney,
    pgBaseFee :: HighPrecMoney,
    pgTax :: HighPrecMoney,
    settlementAmount :: HighPrecMoney,
    currency :: Currency,
    vendorId :: Maybe Text,
    uniqueSplitId :: Maybe Text,
    paymentGateway :: Maybe Text,
    paymentMethod :: Maybe PaymentMethodType,
    paymentMethodSubType :: Maybe Text,
    settlementType :: Maybe SettlementType,
    settlementMode :: Maybe SettlementMode,
    settlementId :: Maybe Text,
    settlementDate :: Maybe UTCTime,
    refundId :: Maybe Text,
    refundArn :: Maybe Text,
    refundDate :: Maybe UTCTime,
    refundAmount :: Maybe HighPrecMoney,
    refundBaseFee :: Maybe HighPrecMoney,
    refundTax :: Maybe HighPrecMoney,
    disputeId :: Maybe Text,
    disputeType :: Maybe DisputeType,
    rawData :: Maybe Value,
    cardIsin :: Maybe Text,
    cardNetwork :: Maybe Text,
    cardType :: Maybe Text
  }
  deriving (Show, Eq, Generic)

data TxnType = ORDER | REFUND
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data TxnStatus = SUCCESS | FAILED
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data SettlementType = CREDIT | DEBIT
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data SettlementMode = GROSS | NET | NETTING
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data PaymentMethodType = UPI | CREDIT_CARD | DEBIT_CARD | NETBANKING | WALLET
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data DisputeType = FRAUD | CONSUMER | PROCESSING_ERROR | OTHER_DISPUTE
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data ParseSettlementResult = ParseSettlementResult
  { reports :: [PaymentSettlementReport],
    totalRows :: Int,
    failedRows :: Int,
    errors :: [Text]
  }
  deriving (Show, Generic)
