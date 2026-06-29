{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.Interface.Types
  ( -- * Common enums (shared across payment & payout)
    TxnStatus (..),
    SettlementType (..),
    SettlementMode (..),

    -- * Generic parse result
    ParseResult (..),

    -- * Payment settlement
    PaymentSettlementReport (..),
    TxnType (..),
    PaymentMethodType (..),
    DisputeType (..),
    ParsePaymentSettlementResult,

    -- * Payout settlement
    PayoutSettlementReport (..),
    FulfillmentInstrument (..),
    ParsePayoutSettlementResult,
  )
where

import Data.Aeson (Value)
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)

-- ---------------------------------------------------------------------------
-- Common enums shared by payment and payout reports
-- ---------------------------------------------------------------------------

data TxnStatus = SUCCESS | FAILED
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data SettlementType = CREDIT | DEBIT
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data SettlementMode = GROSS | NET | NETTING
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Generic parse result (parameterised over report type)
-- ---------------------------------------------------------------------------

data ParseResult a = ParseResult
  { reports :: [a],
    totalRows :: Int,
    failedRows :: Int,
    errors :: [Text]
  }
  deriving (Show, Generic)

-- ---------------------------------------------------------------------------
-- Payment settlement report
-- ---------------------------------------------------------------------------

type ParsePaymentSettlementResult = ParseResult PaymentSettlementReport

data TxnType = ORDER | REFUND | CHARGEBACK
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data PaymentMethodType
  = UPI
  | CREDIT_CARD
  | DEBIT_CARD
  | NETBANKING
  | WALLET
  | CASH_CARD
  | BHARAT_QR
  | EMI
  | NEFT
  | UPI_CREDIT
  | ENACH
  | CBDC
  | UPI_PREPAID_WALLET
  | UPI_CREDIT_LINE
  | BANK_TRANSFER
  | COMMERCIAL_CARD
  | PAY_LATER
  | INTERNATIONAL_CARD
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data DisputeType = FRAUD | CONSUMER | PROCESSING_ERROR | OTHER_DISPUTE
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data PaymentSettlementReport = PaymentSettlementReport
  { orderId :: Text, -- Merchant Subscription Transaction ID
    txnId :: Maybe Text, -- Transaction ID (internal PG txn reference)
    rrn :: Maybe Text, -- Bank RRN
    utr :: Maybe Text, -- Settlement UTR
    txnType :: TxnType, -- Document Type (ORDER/REFUND/CHARGEBACK)
    txnStatus :: TxnStatus, -- Transaction Status
    txnDate :: Maybe UTCTime, -- Transaction Date
    txnAmount :: HighPrecMoney, -- Charged Amount
    pgBaseFee :: HighPrecMoney, -- PG Fee
    pgTax :: HighPrecMoney, -- GST on PG Fee
    settlementAmount :: HighPrecMoney, -- Net Settled Amount
    currency :: Currency, -- Currency
    vendorId :: Maybe Text, -- MID
    uniqueSplitId :: Maybe Text, -- Unique Split ID (HyperPG only)
    paymentGateway :: Maybe Text, -- PG name
    paymentMethod :: Maybe PaymentMethodType, -- Paymode
    paymentMethodSubType :: Maybe Text, -- Payment Method Sub Type
    settlementType :: Maybe SettlementType, -- Settlement Type (CREDIT/DEBIT)
    settlementMode :: Maybe SettlementMode, -- Settlement Mode (GROSS/NET/NETTING)
    settlementId :: Maybe Text, -- Settlement ID
    settlementDate :: Maybe UTCTime, -- Settlement Date
    pgApprovalCode :: Maybe Text, -- PG Approval code
    pgRequestId :: Maybe Text, -- PG Request id
    bankId :: Maybe Text, -- Bank ID
    refundId :: Maybe Text, -- Refund ID
    refundArn :: Maybe Text, -- ARN
    refundDate :: Maybe UTCTime, -- Refund Date
    refundAmount :: Maybe HighPrecMoney, -- Refund Amount
    refundBaseFee :: Maybe HighPrecMoney, -- Refund Base Fee
    refundTax :: Maybe HighPrecMoney, -- Refund Tax
    disputeId :: Maybe Text, -- Dispute ID
    disputeType :: Maybe DisputeType, -- Dispute Type
    rawData :: Maybe Value, -- Raw Data (full CSV row as JSON)
    cardIsin :: Maybe Text, -- Card ISIN
    cardNetwork :: Maybe Text, -- Card Network
    cardType :: Maybe Text, -- Card Type
    isOffer :: Maybe Bool, -- Is Offer Applied
    offerCode :: Maybe Text, -- Offer Code
    offerId :: Maybe Text, -- Offer ID
    actualAmount :: Maybe HighPrecMoney, -- Actual Amount
    cardNumber :: Maybe Text -- Card Number
  }
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Payout settlement report
-- ---------------------------------------------------------------------------

type ParsePayoutSettlementResult = ParseResult PayoutSettlementReport

data FulfillmentInstrument = IMPS | FI_NEFT | RTGS | FI_UPI | OTHER_INSTRUMENT
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data PayoutSettlementReport = PayoutSettlementReport
  { orderId :: Text,
    txnId :: Maybe Text,
    rrn :: Maybe Text,
    utr :: Maybe Text,
    txnStatus :: TxnStatus,
    txnDate :: Maybe UTCTime,
    txnAmount :: HighPrecMoney,
    settlementAmount :: HighPrecMoney,
    currency :: Currency,
    payoutCustomerId :: Text,
    paymentGateway :: Maybe Text,
    beneficiaryIfsc :: Maybe Text,
    beneficiaryAccountNumber :: Maybe Text,
    beneficiaryType :: Maybe Text,
    bankName :: Maybe Text,
    settlementType :: Maybe SettlementType,
    settlementMode :: Maybe SettlementMode,
    settlementId :: Maybe Text,
    settlementDate :: Maybe UTCTime,
    payoutRequestId :: Maybe Text,
    fulfillmentId :: Maybe Text,
    fulfillmentInstrumentType :: Maybe FulfillmentInstrument,
    fulfillmentMethod :: Maybe Text,
    fulfillmentStatus :: Maybe Text,
    fulfillmentResponseCode :: Maybe Text,
    fulfillmentResponseMessage :: Maybe Text,
    fulfillmentDate :: Maybe UTCTime,
    fulfillmentAmount :: Maybe HighPrecMoney,
    rawData :: Maybe Value
  }
  deriving (Show, Eq, Generic)
