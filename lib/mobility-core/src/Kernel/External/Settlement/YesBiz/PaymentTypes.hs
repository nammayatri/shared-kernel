{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.YesBiz.PaymentTypes where

import qualified Data.Aeson as A
import qualified Data.Csv as Csv
import Kernel.Prelude

data YesBizRow = YesBizRow
  { merchantId :: Text,
    transactionAmount :: Text,
    fee :: Text,
    tax :: Text,
    credit :: Text,
    debit :: Text,
    partnerAmount :: Text,
    settlementDate :: Text,
    upiRequestId :: Text,
    transactionType :: Text,
    txnType :: Text,
    transactionDate :: Text,
    merchantAccountNumber :: Text,
    status :: Text,
    payerVpa :: Text,
    payeeVpa :: Text,
    rrn :: Text,
    settlementStatus :: Text,
    orderId :: Text,
    transactionDescription :: Text,
    utrNumber :: Text
  }
  deriving (Show, Eq, Generic)

instance Csv.FromNamedRecord YesBizRow where
  parseNamedRecord r =
    YesBizRow
      <$> r Csv..: "Merchant ID"
      <*> r Csv..: "Transaction Amount"
      <*> r Csv..: "Fee"
      <*> r Csv..: "Tax"
      <*> r Csv..: "Credit"
      <*> r Csv..: "Debit"
      <*> r Csv..: "Partner Amount"
      <*> r Csv..: "Settlement Date"
      <*> r Csv..: "UPI Request Id"
      <*> r Csv..: "Transaction Type"
      <*> r Csv..: "Type"
      <*> r Csv..: "Transaction Date"
      <*> r Csv..: "Merchant Account Number"
      <*> r Csv..: "Status"
      <*> r Csv..: "Payer Vpa"
      <*> r Csv..: "Payee Vpa"
      <*> r Csv..: "RRN"
      <*> r Csv..: "Settlement Status"
      <*> r Csv..: "Order ID"
      <*> r Csv..: "Transaction Description"
      <*> r Csv..: "UTR Number"

instance A.ToJSON YesBizRow where
  toJSON row =
    A.object
      [ "merchantId" A..= row.merchantId,
        "transactionAmount" A..= row.transactionAmount,
        "fee" A..= row.fee,
        "tax" A..= row.tax,
        "credit" A..= row.credit,
        "debit" A..= row.debit,
        "partnerAmount" A..= row.partnerAmount,
        "settlementDate" A..= row.settlementDate,
        "upiRequestId" A..= row.upiRequestId,
        "transactionType" A..= row.transactionType,
        "txnType" A..= row.txnType,
        "transactionDate" A..= row.transactionDate,
        "merchantAccountNumber" A..= row.merchantAccountNumber,
        "status" A..= row.status,
        "payerVpa" A..= row.payerVpa,
        "payeeVpa" A..= row.payeeVpa,
        "rrn" A..= row.rrn,
        "settlementStatus" A..= row.settlementStatus,
        "orderId" A..= row.orderId,
        "transactionDescription" A..= row.transactionDescription,
        "utrNumber" A..= row.utrNumber
      ]
