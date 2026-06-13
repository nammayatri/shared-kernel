{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.CCAvenue.PaymentTypes where

import qualified Data.Aeson as A
import qualified Data.Csv as Csv
import Kernel.Prelude

-- Payout Summary section row
data CCAvenuePayoutRow = CCAvenuePayoutRow
  { payId :: Text,
    utrNo :: Text
  }
  deriving (Show, Eq, Generic)

instance Csv.FromNamedRecord CCAvenuePayoutRow where
  parseNamedRecord r =
    CCAvenuePayoutRow
      <$> r Csv..: "Pay ID"
      <*> r Csv..: "UTR No"

instance A.ToJSON CCAvenuePayoutRow where
  toJSON row =
    A.object
      [ "payId" A..= row.payId,
        "utrNo" A..= row.utrNo
      ]

-- Transaction Summary Details section row
data CCAvenueTxnRow = CCAvenueTxnRow
  { txnDate :: Text,
    settlementDate :: Text,
    ccaOrderId :: Text,
    amount :: Text,
    txnType :: Text,
    tdrAmount :: Text,
    tax :: Text,
    payableAmount :: Text,
    merchantId :: Text,
    ccavenueRefNumber :: Text,
    authCode :: Text,
    invoiceRefNo :: Text,
    paymentType :: Text,
    cardName :: Text
  }
  deriving (Show, Eq, Generic)

instance Csv.FromNamedRecord CCAvenueTxnRow where
  parseNamedRecord r =
    CCAvenueTxnRow
      <$> r Csv..: "Date"
      <*> r Csv..: "Settlement Date"
      <*> r Csv..: "Order ID"
      <*> r Csv..: "Amount"
      <*> r Csv..: "Transaction Type"
      <*> r Csv..: "TDR Amount"
      <*> r Csv..: "Tax"
      <*> r Csv..: "Payable Amount"
      <*> r Csv..: "Merchant ID"
      <*> r Csv..: "CCAvenue Ref Number"
      <*> r Csv..: "Auth Code"
      <*> r Csv..: "Invoice Reference No"
      <*> r Csv..: "Payment Type"
      <*> r Csv..: "Card Name"

instance A.ToJSON CCAvenueTxnRow where
  toJSON row =
    A.object
      [ "txnDate" A..= row.txnDate,
        "settlementDate" A..= row.settlementDate,
        "ccaOrderId" A..= row.ccaOrderId,
        "amount" A..= row.amount,
        "txnType" A..= row.txnType,
        "tdrAmount" A..= row.tdrAmount,
        "tax" A..= row.tax,
        "payableAmount" A..= row.payableAmount,
        "merchantId" A..= row.merchantId,
        "ccavenueRefNumber" A..= row.ccavenueRefNumber,
        "authCode" A..= row.authCode,
        "invoiceRefNo" A..= row.invoiceRefNo,
        "paymentType" A..= row.paymentType,
        "cardName" A..= row.cardName
      ]

-- Merged row after zipping Payout + Transaction sections
data CCAvenueMergedRow = CCAvenueMergedRow
  { txnRow :: CCAvenueTxnRow,
    payoutRow :: Maybe CCAvenuePayoutRow
  }
  deriving (Show, Eq, Generic)

instance A.ToJSON CCAvenueMergedRow where
  toJSON row =
    A.object
      [ "transaction" A..= A.toJSON row.txnRow,
        "payout" A..= (A.toJSON <$> row.payoutRow)
      ]
