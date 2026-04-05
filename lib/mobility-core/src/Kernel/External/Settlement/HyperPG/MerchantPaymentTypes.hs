{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.HyperPG.MerchantPaymentTypes where

import qualified Data.Aeson as A
import qualified Data.Csv as Csv
import Kernel.Prelude

data HyperPGMerchantRow = HyperPGMerchantRow
  { orderId :: Text,
    transactionId :: Text,
    transactionDate :: Text,
    rrn :: Text,
    transactionType :: Text,
    transactionStatus :: Text,
    settlementMode :: Text,
    paymentMethodType :: Text,
    paymentMethodSubType :: Text,
    txnSettlementType :: Text,
    settlementId :: Text,
    settlementDate :: Text,
    utr :: Text,
    amount :: Text,
    fee :: Text,
    tax :: Text,
    settlementAmount :: Text,
    transactionSplits :: Text,
    refundId :: Text,
    refundDate :: Text,
    refundArn :: Text,
    disputeId :: Text,
    disputeType :: Text
  }
  deriving (Show, Eq, Generic)

instance Csv.FromNamedRecord HyperPGMerchantRow where
  parseNamedRecord r =
    HyperPGMerchantRow
      <$> r Csv..: "Order Id"
      <*> r Csv..: "Transaction Id"
      <*> r Csv..: "Transaction Date"
      <*> r Csv..: "RRN"
      <*> r Csv..: "Transaction Type"
      <*> r Csv..: "Transaction Status"
      <*> r Csv..: "Settlement Mode"
      <*> r Csv..: "Payment Method Type"
      <*> r Csv..: "Payment Method Sub Type"
      <*> r Csv..: "Type"
      <*> r Csv..: "Settlement Id"
      <*> r Csv..: "Settlement Date"
      <*> r Csv..: "utr"
      <*> r Csv..: "amount"
      <*> r Csv..: "fee"
      <*> r Csv..: "tax"
      <*> r Csv..: "Settlement Amount"
      <*> r Csv..: "Transaction Splits"
      <*> r Csv..: "Refund Id"
      <*> r Csv..: "Refund Date"
      <*> r Csv..: "Refund Arn"
      <*> r Csv..: "Dispute Id"
      <*> r Csv..: "Dispute Type"

instance A.ToJSON HyperPGMerchantRow where
  toJSON row =
    A.object
      [ "orderId" A..= row.orderId,
        "transactionId" A..= row.transactionId,
        "transactionDate" A..= row.transactionDate,
        "rrn" A..= row.rrn,
        "transactionType" A..= row.transactionType,
        "transactionStatus" A..= row.transactionStatus,
        "settlementMode" A..= row.settlementMode,
        "paymentMethodType" A..= row.paymentMethodType,
        "paymentMethodSubType" A..= row.paymentMethodSubType,
        "txnSettlementType" A..= row.txnSettlementType,
        "settlementId" A..= row.settlementId,
        "settlementDate" A..= row.settlementDate,
        "utr" A..= row.utr,
        "amount" A..= row.amount,
        "fee" A..= row.fee,
        "tax" A..= row.tax,
        "settlementAmount" A..= row.settlementAmount,
        "transactionSplits" A..= row.transactionSplits,
        "refundId" A..= row.refundId,
        "refundDate" A..= row.refundDate,
        "refundArn" A..= row.refundArn,
        "disputeId" A..= row.disputeId,
        "disputeType" A..= row.disputeType
      ]

-- Types for parsing the embedded Transaction Splits JSON

data ComputedVendorSplit = ComputedVendorSplit
  { grossAmount :: Text
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON ComputedVendorSplit where
  parseJSON = A.withObject "ComputedVendorSplit" $ \v ->
    ComputedVendorSplit <$> v A..: "gross_amount"

data TransactionSplits = TransactionSplits
  { computedVendorsSplits :: [ComputedVendorSplit]
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON TransactionSplits where
  parseJSON = A.withObject "TransactionSplits" $ \v ->
    TransactionSplits <$> v A..:? "computed_vendors_splits" A..!= []
