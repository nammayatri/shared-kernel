{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Payment.Interface.Types
  ( module Kernel.External.Payment.Interface.Types,
    module Reexport,
  )
where

import qualified Kernel.External.Payment.Juspay.Config as Juspay
import Kernel.External.Payment.Juspay.Types as Reexport (CreateOrderResp (..), Currency (..), PaymentLinks (..), TransactionStatus (..))
import Kernel.Prelude
import Kernel.Types.Common

newtype PaymentServiceConfig = JuspayConfig Juspay.JuspayCfg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CreateOrderReq = CreateOrderReq
  { orderShortId :: Text, -- Should be Alphanumeric with character length less than 18.
    amount :: HighPrecMoney,
    customerId :: Text,
    customerEmail :: Text,
    customerPhone :: Text,
    paymentPageClientId :: Text,
    customerFirstName :: Maybe Text,
    customerLastName :: Maybe Text
  }

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
