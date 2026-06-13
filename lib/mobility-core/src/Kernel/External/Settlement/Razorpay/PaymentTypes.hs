{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.Razorpay.PaymentTypes where

import qualified Data.Aeson as A
import qualified Data.Csv as Csv
import Kernel.Prelude

data RazorpayRow = RazorpayRow
  { paymentCapturedAt :: Text,
    settledAt :: Text,
    orderReceipt :: Text,
    amount :: Text,
    transactionEntity :: Text,
    fee :: Text,
    tax :: Text,
    settledBy :: Text,
    credit :: Text,
    entityId :: Text,
    rrn :: Text,
    settlementUtr :: Text,
    arn :: Text,
    orderId :: Text,
    issuerName :: Text,
    settlementId :: Text,
    paymentMethod :: Text,
    cardType :: Text,
    vpa :: Text
  }
  deriving (Show, Eq, Generic)

instance Csv.FromNamedRecord RazorpayRow where
  parseNamedRecord r =
    RazorpayRow
      <$> r Csv..: "payment_captured_at"
      <*> r Csv..: "settled_at"
      <*> r Csv..: "order_receipt"
      <*> r Csv..: "amount"
      <*> r Csv..: "transaction_entity"
      <*> r Csv..: "fee"
      <*> r Csv..: "tax"
      <*> r Csv..: "settled_by"
      <*> r Csv..: "credit"
      <*> r Csv..: "entity_id"
      <*> r Csv..: "RRN"
      <*> r Csv..: "settlement_utr"
      <*> r Csv..: "arn"
      <*> r Csv..: "order_id"
      <*> r Csv..: "issuer_name"
      <*> r Csv..: "settlement_id"
      <*> r Csv..: "payment_method"
      <*> r Csv..: "card_type"
      <*> r Csv..: "VPA"

instance A.ToJSON RazorpayRow where
  toJSON row =
    A.object
      [ "paymentCapturedAt" A..= row.paymentCapturedAt,
        "settledAt" A..= row.settledAt,
        "orderReceipt" A..= row.orderReceipt,
        "amount" A..= row.amount,
        "transactionEntity" A..= row.transactionEntity,
        "fee" A..= row.fee,
        "tax" A..= row.tax,
        "settledBy" A..= row.settledBy,
        "credit" A..= row.credit,
        "entityId" A..= row.entityId,
        "rrn" A..= row.rrn,
        "settlementUtr" A..= row.settlementUtr,
        "arn" A..= row.arn,
        "orderId" A..= row.orderId,
        "issuerName" A..= row.issuerName,
        "settlementId" A..= row.settlementId,
        "paymentMethod" A..= row.paymentMethod,
        "cardType" A..= row.cardType,
        "vpa" A..= row.vpa
      ]
