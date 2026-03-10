{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.HyperPG.PayoutTypes where

import qualified Data.Aeson as A
import qualified Data.Csv as Csv
import Kernel.Prelude

data HyperPGPayoutRow = HyperPGPayoutRow
  { fulfillmentUuid :: Text,
    customerId :: Text,
    orderId :: Text,
    amount :: Text,
    fulfillmentTxnStatus :: Text,
    fulfillmentStatus :: Text,
    fulfillmentTxnUuid :: Text,
    transactionReference :: Text,
    bankName :: Text,
    beneficiaryDetails :: Text,
    gatewayRefId :: Text,
    fulfillmentMethod :: Text,
    gateway :: Text,
    fulfillmentInstrumentType :: Text,
    orderStatus :: Text,
    responseCode :: Text,
    responseMessage :: Text,
    fulfillmentTxnCreatedAt :: Text,
    fulfillmentTxnUpdatedAt :: Text,
    fulfillmentCreatedAt :: Text,
    orderCreatedAt :: Text,
    orderUpdatedAt :: Text,
    fulfillmentUpdatedAt :: Text
  }
  deriving (Show, Eq, Generic)

instance Csv.FromNamedRecord HyperPGPayoutRow where
  parseNamedRecord r =
    HyperPGPayoutRow
      <$> r Csv..: "fulfillment_uuid"
      <*> r Csv..: "customer_id"
      <*> r Csv..: "order_id"
      <*> r Csv..: "amount"
      <*> r Csv..: "fulfillment_txn_status"
      <*> r Csv..: "fulfillment_status"
      <*> r Csv..: "fulfillment_txn_uuid"
      <*> r Csv..: "transaction_reference"
      <*> r Csv..: "bank_name"
      <*> r Csv..: "beneficiary_details"
      <*> r Csv..: "gateway_ref_id"
      <*> r Csv..: "fulfillment_method"
      <*> r Csv..: "gateway"
      <*> r Csv..: "fulfillment_instrument_type"
      <*> r Csv..: "order_status"
      <*> r Csv..: "response_code"
      <*> r Csv..: "response_message"
      <*> r Csv..: "fulfillment_txn_created_at"
      <*> r Csv..: "fulfillment_txn_updated_at"
      <*> r Csv..: "fulfillment_created_at"
      <*> r Csv..: "order_created_at"
      <*> r Csv..: "order_updated_at"
      <*> r Csv..: "fulfillment_updated_at"

instance A.ToJSON HyperPGPayoutRow where
  toJSON row =
    A.object
      [ "fulfillmentUuid" A..= row.fulfillmentUuid,
        "customerId" A..= row.customerId,
        "orderId" A..= row.orderId,
        "amount" A..= row.amount,
        "fulfillmentTxnStatus" A..= row.fulfillmentTxnStatus,
        "fulfillmentStatus" A..= row.fulfillmentStatus,
        "fulfillmentTxnUuid" A..= row.fulfillmentTxnUuid,
        "transactionReference" A..= row.transactionReference,
        "bankName" A..= row.bankName,
        "beneficiaryDetails" A..= row.beneficiaryDetails,
        "gatewayRefId" A..= row.gatewayRefId,
        "fulfillmentMethod" A..= row.fulfillmentMethod,
        "gateway" A..= row.gateway,
        "fulfillmentInstrumentType" A..= row.fulfillmentInstrumentType,
        "orderStatus" A..= row.orderStatus,
        "responseCode" A..= row.responseCode,
        "responseMessage" A..= row.responseMessage,
        "fulfillmentTxnCreatedAt" A..= row.fulfillmentTxnCreatedAt,
        "fulfillmentTxnUpdatedAt" A..= row.fulfillmentTxnUpdatedAt,
        "fulfillmentCreatedAt" A..= row.fulfillmentCreatedAt,
        "orderCreatedAt" A..= row.orderCreatedAt,
        "orderUpdatedAt" A..= row.orderUpdatedAt,
        "fulfillmentUpdatedAt" A..= row.fulfillmentUpdatedAt
      ]
