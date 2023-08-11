{-# LANGUAGE DerivingStrategies #-}
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
import Kernel.External.Payment.Juspay.Types as Reexport (CreateOrderResp (..), Currency (..), MandateFrequency (..), MandateStatus (..), MandateType (..), OfferListStatus (..), OfferStatus (..), PaymentLinks (..), TransactionStatus (..))
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common

newtype PaymentServiceConfig = JuspayConfig Juspay.JuspayCfg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CreateOrderReq = CreateOrderReq
  { orderId :: Text,
    orderShortId :: Text, -- Should be Alphanumeric with character length less than 18.
    amount :: HighPrecMoney,
    customerId :: Text,
    customerEmail :: Text,
    customerPhone :: Text,
    customerFirstName :: Maybe Text,
    customerLastName :: Maybe Text,
    createMandate :: Maybe MandateType,
    mandateMaxAmount :: Maybe HighPrecMoney,
    mandateFrequency :: Maybe MandateFrequency
  }

newtype OrderStatusReq = OrderStatusReq
  { orderShortId :: Text
  }

data OrderStatusResp
  = OrderStatusResp
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
  | MandateOrderStatusResp
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
        dateCreated :: Maybe UTCTime,
        mandateStartDate :: UTCTime,
        mandateEndDate :: UTCTime,
        mandateId :: Text,
        mandateStatus :: MandateStatus,
        mandateFrequency :: MandateFrequency,
        mandateMaxAmount :: HighPrecMoney
      }
  | MandateStatusResp
      { status :: MandateStatus,
        mandateStartDate :: UTCTime,
        mandateEndDate :: UTCTime,
        mandateId :: Text,
        mandateFrequency :: MandateFrequency,
        mandateMaxAmount :: HighPrecMoney
      }
  | BadStatusResp
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- notification request --
data MandateNotificationReq = MandateNotificationReq
  { amount :: HighPrecMoney,
    txnDate :: UTCTime,
    mandateId :: Text,
    notificationId :: Text
  }
  deriving (Eq, Show, Generic)

-- mandate pause | resume | revoke request --

data MandateCommandsReq = Pause MandatePauseReq | Resume MandateResumeReq | Revoke MandateRevokeReq

data MandatePauseReq = MandatePauseReq {mandateId :: Text, pauseStartDate :: UTCTime, pauseEndDate :: Maybe UTCTime}

data MandateResumeReq = MandateResumeReq {mandateId :: Text, resumeDate :: UTCTime}

newtype MandateRevokeReq = MandateRevokeReq {mandateId :: Text}

--- mandate Execution request ---

data MandateExecutionReq = MandateExecutionReq
  { notificationId :: Text,
    orderId :: Text,
    amount :: Text,
    customerId :: Text,
    merchantId :: Text,
    mandateId :: Text,
    executionDate :: UTCTime
  }

-- offer list request --

data OfferListReq = OfferListReq
  { order :: OfferOrder,
    customer :: Maybe OfferCustomer,
    planId :: Text,
    registrationDate :: UTCTime
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
    status :: OfferListStatus,
    offerDescription :: OfferDescription,
    orderAmount :: HighPrecMoney,
    finalOrderAmount :: HighPrecMoney,
    discountAmount :: HighPrecMoney
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
    amount :: HighPrecMoney,
    currency :: Currency,
    planId :: Text,
    registrationDate :: UTCTime
  }

newtype OfferApplyResp = OfferApplyResp
  { offers :: [OfferApplyRespItem]
  }

data OfferApplyRespItem = OfferApplyRespItem
  { offerId :: Text,
    finalOrderAmount :: HighPrecMoney
  }

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

type OfferNotifyResp = APISuccess
