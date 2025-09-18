{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Juspay.Types.CreateOrder where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Kernel.External.Payment.Juspay.Types.Common
import Kernel.Prelude
import Kernel.Types.Price
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Web.FormUrlEncoded

data CreateOrderReq = CreateOrderReq
  { order_id :: Text,
    amount :: Text,
    customer_id :: Text,
    customer_email :: Text,
    customer_phone :: Text,
    payment_page_client_id :: Text,
    action :: Text,
    return_url :: Text,
    description :: Text,
    first_name :: Maybe Text,
    last_name :: Maybe Text,
    create_mandate :: Maybe MandateType,
    mandate_max_amount :: Maybe Text,
    mandate_frequency :: Maybe MandateFrequency,
    metadata_mandate_name :: Maybe Text,
    metadata_remarks :: Text,
    mandate_start_date :: Maybe Text,
    mandate_end_date :: Maybe Text,
    metadata_gateway_reference_id :: Maybe Text,
    options_get_upi_deep_links :: Maybe Bool,
    metadata_expiry_in_mins :: Maybe Int,
    split_settlement_details :: Maybe SplitSettlementDetails
  }
  deriving stock (Show, Eq, Generic)

data SplitSettlementDetails
  = AmountBased SplitSettlementDetailsAmount
  | PercentageBased SplitSettlementDetailsPercentage
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- instance FromJSON SplitSettlementDetails where
--   parseJSON v = (AmountBased <$> parseJSON v) <|> (PercentageBased <$> parseJSON v)

-- instance ToJSON SplitSettlementDetails where
--   toJSON (AmountBased details) = toJSON details
--   toJSON (PercentageBased details) = toJSON details

-- instance FromHttpApiData SplitSettlementDetails where
--   parseUrlPiece = parseHeader . DT.encodeUtf8
--   parseQueryParam = parseUrlPiece
--   parseHeader = left T.pack . eitherDecode . BSL.fromStrict

-- instance ToHttpApiData SplitSettlementDetails where
--   toUrlPiece = DT.decodeUtf8 . toHeader
--   toQueryParam = toUrlPiece
--   toHeader = BSL.toStrict . encode

data SplitSettlementDetailsAmount = SplitSettlementDetailsAmount
  { marketplace :: MarketplaceAmount,
    mdr_borne_by :: Text,
    vendor :: VendorAmount
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromHttpApiData SplitSettlementDetailsAmount where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData SplitSettlementDetailsAmount where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

newtype MarketplaceAmount = MarketplaceAmount
  { amount :: HighPrecMoney
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype VendorAmount = VendorAmount
  { split :: [SplitAmount]
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SplitAmount = SplitAmount
  { amount :: HighPrecMoney,
    merchant_commission :: HighPrecMoney,
    sub_mid :: Text,
    unique_split_id :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MBY = MARKETPLACE | VENDOR | ALL
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SplitSettlementDetailsPercentage = SplitSettlementDetailsPercentage
  { marketplace :: MarketplacePercentage,
    mdr_borne_by :: Text,
    vendor :: VendorPercentage
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromHttpApiData SplitSettlementDetailsPercentage where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData SplitSettlementDetailsPercentage where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

newtype MarketplacePercentage = MarketplacePercentage
  { amount_percentage :: Double
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype VendorPercentage = VendorPercentage
  { split :: [SplitPercentage]
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SplitPercentage = SplitPercentage
  { amount_percentage :: Double,
    merchant_commission_percentage :: Double,
    sub_mid :: Text,
    unique_split_id :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

jsonReqOptions :: Options
jsonReqOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "create_mandate" -> "options.create_mandate"
        "mandate_max_amount" -> "mandate.max_amount"
        "mandate_frequency" -> "mandate.frequency"
        "mandate_start_date" -> "mandate.start_date"
        "mandate_end_date" -> "mandate.end_date"
        "metadata_mandate_name" -> "metadata.AXIS_BIZ:mandate_name"
        "metadata_remarks" -> "metadata.AXIS_BIZ:remarks"
        "metadata_gateway_reference_id" -> "metadata.JUSPAY:gateway_reference_id"
        "metadata_expiry_in_mins" -> "metadata.expiryInMins"
        "split_settlement_details" -> "metadata.split_settlement_details"
        other -> other
    }

instance FromJSON CreateOrderReq where
  parseJSON = genericParseJSON jsonReqOptions {omitNothingFields = True}

instance ToJSON CreateOrderReq where
  toJSON = genericToJSON jsonReqOptions {omitNothingFields = True}

data CreateOrderResp = CreateOrderResp
  { status :: TransactionStatus,
    id :: Text,
    order_id :: Text,
    payment_links :: Maybe PaymentLinks,
    sdk_payload :: SDKPayload,
    sdk_payload_json :: Maybe Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON CreateOrderResp where
  parseJSON = withObject "CreateOrderResp" $ \v -> do
    status <- v .: "status"
    order_id <- v .: "order_id"
    id <- v .: "id"
    payment_links <- v .: "payment_links"
    sdk_payload <- v .: "sdk_payload"
    sdk_payload_json <- v .: "sdk_payload"
    return (CreateOrderResp status id order_id payment_links sdk_payload sdk_payload_json)

data PaymentLinks = PaymentLinks
  { web :: Maybe BaseUrl,
    iframe :: Maybe BaseUrl,
    mobile :: Maybe BaseUrl,
    deep_link :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data SDKPayload = SDKPayload
  { requestId :: Maybe Text,
    service :: Maybe Text,
    payload :: SDKPayloadDetails
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data SDKPayloadDetails = SDKPayloadDetails
  { clientId :: Maybe Text,
    amount :: Text,
    merchantId :: Maybe Text,
    clientAuthToken :: Text,
    clientAuthTokenExpiry :: UTCTime,
    environment :: Maybe Text,
    options_getUpiDeepLinks :: Maybe Bool,
    lastName :: Maybe Text,
    action :: Maybe Text,
    customerId :: Maybe Text,
    returnUrl :: Maybe Text,
    currency :: Currency,
    firstName :: Maybe Text,
    customerPhone :: Maybe Text,
    customerEmail :: Maybe Text,
    orderId :: Maybe Text,
    description :: Maybe Text,
    createMandate :: Maybe MandateType,
    mandateMaxAmount :: Maybe Text,
    mandateStartDate :: Maybe Text,
    mandateEndDate :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "createMandate" -> "options.createMandate"
        "mandateMaxAmount" -> "mandate.maxAmount"
        "mandateStartDate" -> "mandate.startDate"
        "mandateEndDate" -> "mandate.endDate"
        "options_getUpiDeepLinks" -> "options.getUpiDeepLinks"
        "split_settlement_details" -> "metadata.split_settlement_details"
        other -> other
    }

instance FromJSON SDKPayloadDetails where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SDKPayloadDetails where
  toJSON = genericToJSON jsonOptions

data AutoRefundReq = AutoRefundReq
  { amount :: Double,
    unique_request_id :: Text,
    split_settlement_details :: Maybe SplitSettlementDetailsAmount
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (ToSchema)

instance ToForm AutoRefundReq where
  toForm AutoRefundReq {..} =
    Form $
      HM.fromList
        [ ("unique_request_id", [toQueryParam unique_request_id]),
          ("amount", [toQueryParam amount]),
          ("metadata.split_settlement_details", [toQueryParam split_settlement_details])
        ]

data AutoRefundResp = AutoRefundResp
  { order_id :: Text,
    merchant_id :: Text,
    customer_id :: Text,
    currency :: Currency,
    amount_refunded :: Double,
    refunds :: Maybe [RefundsData]
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance FromJSON AutoRefundReq where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AutoRefundReq where
  toJSON = genericToJSON jsonOptions
