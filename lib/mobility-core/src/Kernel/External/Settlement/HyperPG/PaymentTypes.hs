{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.HyperPG.PaymentTypes where

import qualified Data.Aeson as A
import qualified Data.Csv as Csv
import Kernel.Prelude

data HyperPGRow = HyperPGRow
  { orderId :: Text,
    transactionId :: Text,
    transactionDate :: Text,
    rrn :: Text,
    orderType :: Text,
    transactionType :: Text,
    transactionStatus :: Text,
    settlementMode :: Text,
    paymentMethodType :: Text,
    paymentMethodSubType :: Text,
    refundId :: Text,
    refundDate :: Text,
    holdStatus :: Text,
    txnSettlementType :: Text,
    disputeId :: Text,
    disputeType :: Text,
    overallTxnAmount :: Text,
    overallFee :: Text,
    overallTax :: Text,
    overallSettlementAmount :: Text,
    vendorId :: Text,
    uniqueSplitId :: Text,
    vendorSettlementId :: Text,
    vendorUtr :: Text,
    vendorTxnAmount :: Text,
    vendorMdrFees :: Text,
    vendorTaxAmount :: Text,
    merchantCommission :: Text,
    vendorSettlementAmount :: Text,
    vendorSettlementDate :: Text,
    udf1 :: Text,
    udf2 :: Text,
    udf3 :: Text,
    udf4 :: Text,
    udf5 :: Text
  }
  deriving (Show, Eq, Generic)

instance Csv.FromNamedRecord HyperPGRow where
  parseNamedRecord r =
    HyperPGRow
      <$> r Csv..: "Order Id"
      <*> r Csv..: "Transaction Id"
      <*> r Csv..: "Transaction Date"
      <*> r Csv..: "RRN"
      <*> r Csv..: "Order Type"
      <*> r Csv..: "Transaction Type"
      <*> r Csv..: "Transaction Status"
      <*> r Csv..: "Settlement Mode"
      <*> r Csv..: "Payment Method Type"
      <*> r Csv..: "Payment Method Sub Type"
      <*> r Csv..: "Refund Id"
      <*> r Csv..: "Refund Date"
      <*> r Csv..: "Hold Status"
      <*> r Csv..: "Type"
      <*> r Csv..: "Dispute Id"
      <*> r Csv..: "Dispute Type"
      <*> r Csv..: "Overall Txn Amount"
      <*> r Csv..: "Overall Fee"
      <*> r Csv..: "Overall Tax"
      <*> r Csv..: "Overall Settlement Amount"
      <*> r Csv..: "Vendor Id"
      <*> r Csv..: "Unique Split Id"
      <*> r Csv..: "Vendor Settlement Id"
      <*> r Csv..: "Vendor Utr"
      <*> r Csv..: "Vendor Txn Amount"
      <*> r Csv..: "Vendor MDR Fees"
      <*> r Csv..: "Vendor Tax Amount"
      <*> r Csv..: "Merchant Commission"
      <*> r Csv..: "Vendor Settlement Amount"
      <*> r Csv..: "Vendor Settlement Date"
      <*> r Csv..: "UDF1"
      <*> r Csv..: "UDF2"
      <*> r Csv..: "UDF3"
      <*> r Csv..: "UDF4"
      <*> r Csv..: "UDF5"

instance A.ToJSON HyperPGRow where
  toJSON row =
    A.object
      [ "orderId" A..= row.orderId,
        "transactionId" A..= row.transactionId,
        "transactionDate" A..= row.transactionDate,
        "rrn" A..= row.rrn,
        "orderType" A..= row.orderType,
        "transactionType" A..= row.transactionType,
        "transactionStatus" A..= row.transactionStatus,
        "settlementMode" A..= row.settlementMode,
        "paymentMethodType" A..= row.paymentMethodType,
        "paymentMethodSubType" A..= row.paymentMethodSubType,
        "refundId" A..= row.refundId,
        "refundDate" A..= row.refundDate,
        "holdStatus" A..= row.holdStatus,
        "txnSettlementType" A..= row.txnSettlementType,
        "disputeId" A..= row.disputeId,
        "disputeType" A..= row.disputeType,
        "overallTxnAmount" A..= row.overallTxnAmount,
        "overallFee" A..= row.overallFee,
        "overallTax" A..= row.overallTax,
        "overallSettlementAmount" A..= row.overallSettlementAmount,
        "vendorId" A..= row.vendorId,
        "uniqueSplitId" A..= row.uniqueSplitId,
        "vendorSettlementId" A..= row.vendorSettlementId,
        "vendorUtr" A..= row.vendorUtr,
        "vendorTxnAmount" A..= row.vendorTxnAmount,
        "vendorMdrFees" A..= row.vendorMdrFees,
        "vendorTaxAmount" A..= row.vendorTaxAmount,
        "merchantCommission" A..= row.merchantCommission,
        "vendorSettlementAmount" A..= row.vendorSettlementAmount,
        "vendorSettlementDate" A..= row.vendorSettlementDate
      ]
