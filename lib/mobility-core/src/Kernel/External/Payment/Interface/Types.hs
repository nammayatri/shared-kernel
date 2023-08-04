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
import Kernel.External.Payment.Juspay.Types as Reexport (CreateOrderResp (..), Currency (..), OfferStatus (..), PaymentLinks (..), TransactionStatus (..))
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common

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

-- offer list request --

data OfferListReq = OfferListReq
  { order :: OfferOrder,
    customer :: Maybe OfferCustomer
  }

data OfferOrder = OfferOrder
  { orderId :: Maybe Text,
    amount :: HighPrecMoney,
    currency :: Currency
  }

data OfferCustomer = OfferCustomer
  { customerId :: Text,
    email :: Maybe Text,
    mobile :: Maybe Text
  }

-- offer list response --

data OfferListResp = OfferListResp
  { bestOfferCombination :: Maybe BestOfferCombination,
    offerResp :: [OfferResp]
  }

data BestOfferCombination = BestOfferCombination
  { offers :: [BestOfferCombinationOffer],
    orderBreakup :: OrderBreakup
  }

data BestOfferCombinationOffer = BestOfferCombinationOffer
  { offerId :: Text,
    cashbackAmount :: HighPrecMoney,
    discountAmount :: HighPrecMoney,
    merchantDiscountAmount :: HighPrecMoney,
    totalOfferedAmount :: HighPrecMoney
  }

data OrderBreakup = OrderBreakup
  { orderAmount :: HighPrecMoney,
    finalOrderAmount :: HighPrecMoney,
    discountAmount :: HighPrecMoney,
    merchantDiscountAmount :: HighPrecMoney,
    cashbackAmount :: HighPrecMoney,
    offerAmount :: HighPrecMoney
  }

data OfferResp = OfferResp
  { offerId :: Text,
    status :: OfferStatus,
    offerDescription :: OfferDescription
  }

data OfferDescription = OfferDescription
  { sponsoredBy :: Maybe Text,
    title :: Maybe Text,
    description :: Maybe Text,
    tnc :: Maybe Text
  }

-- offer apply --

data OfferApplyReq = OfferApplyReq
  { mandateId :: Text,
    orderShortId :: Text,
    offers :: [Text],
    customerId :: Text,
    amount :: Money,
    currency :: Currency
  }

type OfferApplyResp = APISuccess -- FIXME

-- offer notify --

data OfferNotifyReq = OfferNotifyReq
  { mandateId :: Text,
    orderShortId :: Text,
    transactionUUID :: Text,
    transactionStatus :: TransactionStatus,
    offers :: [OfferNotifyOffer]
  }

data OfferNotifyOffer = OfferNotifyOffer
  { offerId :: Text,
    status :: OfferStatus
  }

type OfferNotifyResp = APISuccess -- FIXME
