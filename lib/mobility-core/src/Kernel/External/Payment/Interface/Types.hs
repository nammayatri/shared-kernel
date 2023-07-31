{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLists #-}

module Kernel.External.Payment.Interface.Types
  ( module Kernel.External.Payment.Interface.Types,
    module Reexport,
  )
where

import qualified Kernel.External.Payment.Juspay.Config as Juspay
import Kernel.External.Payment.Juspay.Types as Reexport (CreateOrderResp (..), Currency (..), PaymentLinks (..), TransactionStatus (..))
import Kernel.Prelude
import Kernel.Types.Common
import Web.FormUrlEncoded

-- import Servant (ToHttpApiData(toQueryParam))
-- import Web.Internal.HttpApiData

newtype PaymentServiceConfig = JuspayConfig Juspay.JuspayCfg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CreateOrderReq = CreateOrderReq
  { orderShortId :: Text, -- Should be Alphanumeric with character length less than 18.
    amount :: Money,
    customerId :: Text,
    customerEmail :: Text,
    customerPhone :: Text,
    paymentPageClientId :: Text,
    customerFirstName :: Maybe Text,
    customerLastName :: Maybe Text
  }

data RegisterMandateReq = RegisterMandateReq
  { order_id :: Text,
    merchant_id :: Text,
    payment_method_type :: Text,
    payment_method :: Text,
    upi_vpa :: Text,
    redirect_after_payment :: Bool,
    format :: Text,
    mandate_type :: Text,
    should_create_mandate :: Bool
  }
  deriving (Eq, Show, Generic, ToForm)

-- instance ToForm RegisterMandateReq where
--   toForm RegisterMandateReq {..} =
--     [ ("order_id", toQueryParam order_id),
--       ("merchant_id", toQueryParam merchant_id),
--       ("payment_method_type", toQueryParam payment_method_type),
--       ("payment_method", toQueryParam payment_method),
--       ("upi_vpa", toQueryParam upi_vpa)
--       -- ("redirect_after_payment", toQueryParam (T.pack $ show redirect_after_payment)),
--       -- ("format", toQueryParam format),
--       -- ("mandate_type", toQueryParam mandate_type),
--       -- ("should_create_mandate", toQueryParam (T.pack $ show should_create_mandate))
--     ]

data RegisterMandateResp = RegisterMandateRequestResp
  { status :: String,
    paymentAuthenticationMethod :: Maybe String,
    paymentAuthenticationUrl :: Maybe String,
    paymentAuthenticationParams :: Maybe String
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

newtype OrderStatusReq = OrderStatusReq
  { orderShortId :: Text
  }

data OrderStatusResp = OrderStatusResp
  { orderShortId :: Text,
    transactionUUID :: Maybe Text,
    transactionStatusId :: Int,
    transactionStatus :: TransactionStatus,
    paymentMethodType :: Maybe Text,
    paymentMethod :: Maybe Text,
    respMessage :: Maybe Text,
    respCode :: Maybe Text,
    gatewayReferenceId :: Maybe Text,
    amount :: HighPrecMoney,
    currency :: Currency,
    dateCreated :: Maybe UTCTime
  }
