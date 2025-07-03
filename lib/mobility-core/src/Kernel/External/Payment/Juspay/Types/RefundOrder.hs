{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Juspay.Types.RefundOrder where

import Data.Aeson
import Kernel.External.Payment.Juspay.Types.CreateOrder (SplitSettlementDetails)
import Kernel.Prelude
import Web.FormUrlEncoded

data RefundOrderReq = RefundOrderReq
  { unique_request_id :: Text,
    amount :: Text,
    split_settlement_details :: Maybe SplitSettlementDetails
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

jsonReqOptionsRefundOrder :: Options
jsonReqOptionsRefundOrder =
  defaultOptions
    { fieldLabelModifier = \case
        "split_settlement_details" -> "metadata.split_settlement_details"
        other -> other
    }

instance FromJSON RefundOrderReq where
  parseJSON = genericParseJSON jsonReqOptionsRefundOrder {omitNothingFields = True}

instance ToJSON RefundOrderReq where
  toJSON = genericToJSON jsonReqOptionsRefundOrder {omitNothingFields = True}

data RefundOrderResp = RefundOrderResp
  { status_id :: Int,
    status :: Text,
    return_url :: Text,
    refunded :: Bool,
    order_id :: Text,
    merchant_id :: Text,
    id :: Text,
    amount :: Double,
    amount_refunded :: Double,
    payment_links :: Maybe PaymentLinks,
    refunds :: Maybe [RefundsData],
    txn_uuid :: Maybe Text,
    txn_id :: Maybe Text,
    txn_detail :: Maybe TxnDetail,
    payment_gateway_response :: Maybe PaymentGatewayResponse,
    customer_phone :: Maybe Text,
    customer_id :: Maybe Text,
    customer_email :: Maybe Text,
    currency :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PaymentLinks = PaymentLinks
  { web :: Maybe BaseUrl,
    iframe :: Maybe BaseUrl,
    mobile :: Maybe BaseUrl
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RefundsData = RefundsData
  { unique_request_id :: Maybe Text,
    status :: Maybe Text,
    sent_to_gateway :: Maybe Bool,
    refund_type :: Maybe Text,
    refund_source :: Maybe Text,
    ref :: Maybe Text,
    initiated_by :: Maybe Text,
    id :: Maybe Text,
    error_message :: Maybe Text,
    error_code :: Maybe Text,
    created :: Maybe UTCTime,
    amount :: Maybe Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TxnDetail = TxnDetail
  { txn_uuid :: Maybe Text,
    txn_id :: Maybe Text,
    txn_amount :: Maybe Double,
    tax_amount :: Maybe Double,
    surcharge_amount :: Maybe Double,
    status :: Maybe Text,
    redirect :: Maybe Bool,
    order_id :: Maybe Text,
    net_amount :: Maybe Double,
    gateway_id :: Maybe Int,
    gateway :: Maybe Text,
    express_checkout :: Maybe Bool,
    error_message :: Maybe Text,
    error_code :: Maybe Text,
    currency :: Maybe Text,
    created :: Maybe UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PaymentGatewayResponse = PaymentGatewayResponse
  { txn_id :: Maybe Text,
    rrn :: Maybe Text,
    resp_message :: Maybe Text,
    resp_code :: Maybe Text,
    epg_txn_id :: Maybe Text,
    created :: Maybe UTCTime,
    auth_id_code :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
