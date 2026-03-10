{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.BillDesk.PaymentTypes where

import qualified Data.Aeson as A
import qualified Data.Csv as Csv
import Kernel.Prelude

-- | Settled transaction row from the "SETTLED TRANSACTIONS" section.
data BillDeskSettledRow = BillDeskSettledRow
  { billerId :: Text,
    bankId :: Text,
    bankRefNo :: Text,
    pgiRefNo :: Text,
    ref1 :: Text,
    ref2 :: Text,
    ref3 :: Text,
    ref4 :: Text,
    ref5 :: Text,
    ref6 :: Text,
    ref7 :: Text,
    ref8 :: Text,
    filler :: Text,
    dateOfTxn :: Text,
    settlementDate :: Text,
    grossAmount :: Text,
    charges :: Text,
    gst :: Text,
    netAmount :: Text
  }
  deriving (Show, Eq, Generic)

instance Csv.FromNamedRecord BillDeskSettledRow where
  parseNamedRecord r =
    BillDeskSettledRow
      <$> r Csv..: "Biller Id"
      <*> r Csv..: "Bank Id"
      <*> r Csv..: "Bank Ref. No."
      <*> r Csv..: "PGI Ref. No."
      <*> r Csv..: "Ref. 1"
      <*> r Csv..: "Ref. 2"
      <*> r Csv..: "Ref. 3"
      <*> r Csv..: "Ref. 4"
      <*> r Csv..: "Ref. 5"
      <*> r Csv..: "Ref. 6"
      <*> r Csv..: "Ref. 7"
      <*> r Csv..: "Ref. 8"
      <*> r Csv..: "Filler"
      <*> r Csv..: "Date of Txn"
      <*> r Csv..: "Settlement Date"
      <*> r Csv..: "Gross Amount(Rs.Ps)"
      <*> r Csv..: "Charges (Rs.Ps)"
      <*> r Csv..: "GST (Rs Ps)"
      <*> r Csv..: "Net Amount(Rs.Ps)"

instance A.ToJSON BillDeskSettledRow where
  toJSON row =
    A.object
      [ "billerId" A..= row.billerId,
        "bankId" A..= row.bankId,
        "bankRefNo" A..= row.bankRefNo,
        "pgiRefNo" A..= row.pgiRefNo,
        "ref1" A..= row.ref1,
        "dateOfTxn" A..= row.dateOfTxn,
        "settlementDate" A..= row.settlementDate,
        "grossAmount" A..= row.grossAmount,
        "charges" A..= row.charges,
        "gst" A..= row.gst,
        "netAmount" A..= row.netAmount,
        "section" A..= ("SETTLED" :: Text)
      ]

-- | Refund transaction row from the "REFUND TRANSACTIONS" section.
data BillDeskRefundRow = BillDeskRefundRow
  { billerId :: Text,
    bankId :: Text,
    bankRefNo :: Text,
    pgiRefNo :: Text,
    ref1 :: Text,
    ref2 :: Text,
    ref3 :: Text,
    ref4 :: Text,
    ref5 :: Text,
    ref6 :: Text,
    ref7 :: Text,
    ref8 :: Text,
    filler :: Text,
    dateOfTxn :: Text,
    settlementDate :: Text,
    grossAmount :: Text,
    refundId :: Text,
    refundDate :: Text,
    refundAmount :: Text
  }
  deriving (Show, Eq, Generic)

instance Csv.FromNamedRecord BillDeskRefundRow where
  parseNamedRecord r =
    BillDeskRefundRow
      <$> r Csv..: "Biller Id"
      <*> r Csv..: "Bank Id"
      <*> r Csv..: "Bank Ref. No."
      <*> r Csv..: "PGI Ref. No."
      <*> r Csv..: "Ref. 1"
      <*> r Csv..: "Ref. 2"
      <*> r Csv..: "Ref. 3"
      <*> r Csv..: "Ref. 4"
      <*> r Csv..: "Ref. 5"
      <*> r Csv..: "Ref. 6"
      <*> r Csv..: "Ref. 7"
      <*> r Csv..: "Ref. 8"
      <*> r Csv..: "Filler"
      <*> r Csv..: "Date of Transaction"
      <*> r Csv..: "Settlement Date"
      <*> r Csv..: "Gross Amount(Rs.Ps)"
      <*> r Csv..: "Refund ID"
      <*> r Csv..: "Refund Date"
      <*> r Csv..: "Refund Amount (Rs. Ps.)"

instance A.ToJSON BillDeskRefundRow where
  toJSON row =
    A.object
      [ "billerId" A..= row.billerId,
        "bankId" A..= row.bankId,
        "bankRefNo" A..= row.bankRefNo,
        "pgiRefNo" A..= row.pgiRefNo,
        "ref1" A..= row.ref1,
        "dateOfTxn" A..= row.dateOfTxn,
        "settlementDate" A..= row.settlementDate,
        "grossAmount" A..= row.grossAmount,
        "refundId" A..= row.refundId,
        "refundDate" A..= row.refundDate,
        "refundAmount" A..= row.refundAmount,
        "section" A..= ("REFUND" :: Text)
      ]

-- | Chargeback transaction row from the "CHARGEBACK TRANSACTIONS" section.
data BillDeskChargebackRow = BillDeskChargebackRow
  { billerId :: Text,
    bankId :: Text,
    bankRefNo :: Text,
    pgiRefNo :: Text,
    ref1 :: Text,
    ref2 :: Text,
    ref3 :: Text,
    ref4 :: Text,
    ref5 :: Text,
    ref6 :: Text,
    ref7 :: Text,
    ref8 :: Text,
    filler :: Text,
    dateOfTxn :: Text,
    settlementDate :: Text,
    grossAmount :: Text,
    chargebackReason :: Text,
    chargebackDate :: Text,
    chargebackAmount :: Text
  }
  deriving (Show, Eq, Generic)

instance Csv.FromNamedRecord BillDeskChargebackRow where
  parseNamedRecord r =
    BillDeskChargebackRow
      <$> r Csv..: "Biller Id"
      <*> r Csv..: "Bank Id"
      <*> r Csv..: "Bank Ref. No."
      <*> r Csv..: "PGI Ref. No."
      <*> r Csv..: "Ref. 1"
      <*> r Csv..: "Ref. 2"
      <*> r Csv..: "Ref. 3"
      <*> r Csv..: "Ref. 4"
      <*> r Csv..: "Ref. 5"
      <*> r Csv..: "Ref. 6"
      <*> r Csv..: "Ref. 7"
      <*> r Csv..: "Ref. 8"
      <*> r Csv..: "Filler"
      <*> r Csv..: "Date of Transaction"
      <*> r Csv..: "Settlement Date"
      <*> r Csv..: "Gross Amount(Rs.Ps)"
      <*> r Csv..: "Chargeback Reason"
      <*> r Csv..: "Chargeback date"
      <*> r Csv..: "Chargeback Amount (Rs Ps)"

instance A.ToJSON BillDeskChargebackRow where
  toJSON row =
    A.object
      [ "billerId" A..= row.billerId,
        "bankId" A..= row.bankId,
        "bankRefNo" A..= row.bankRefNo,
        "pgiRefNo" A..= row.pgiRefNo,
        "ref1" A..= row.ref1,
        "dateOfTxn" A..= row.dateOfTxn,
        "settlementDate" A..= row.settlementDate,
        "grossAmount" A..= row.grossAmount,
        "chargebackReason" A..= row.chargebackReason,
        "chargebackDate" A..= row.chargebackDate,
        "chargebackAmount" A..= row.chargebackAmount,
        "section" A..= ("CHARGEBACK" :: Text)
      ]
