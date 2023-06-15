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
import Kernel.External.Payment.Juspay.Types.Common
import Kernel.Prelude

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
    last_name :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreateOrderResp = CreateOrderResp
  { status :: TransactionStatus,
    id :: Text,
    order_id :: Text,
    payment_links :: Maybe PaymentLinks,
    sdk_payload :: SDKPayload
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PaymentLinks = PaymentLinks
  { web :: Maybe BaseUrl,
    iframe :: Maybe BaseUrl,
    mobile :: Maybe BaseUrl
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data SDKPayload = SDKPayload
  { requestId :: Text,
    service :: Text,
    payload :: SDKPayloadDetails
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data SDKPayloadDetails = SDKPayloadDetails
  { clientId :: Text,
    amount :: Text,
    merchantId :: Text,
    clientAuthToken :: Text,
    clientAuthTokenExpiry :: UTCTime,
    environment :: Maybe Text,
    options_getUpiDeepLinks :: Maybe Bool,
    lastName :: Text,
    action :: Text,
    customerId :: Text,
    returnUrl :: Text,
    currency :: Currency,
    firstName :: Text,
    customerPhone :: Text,
    customerEmail :: Text,
    orderId :: Text,
    description :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
