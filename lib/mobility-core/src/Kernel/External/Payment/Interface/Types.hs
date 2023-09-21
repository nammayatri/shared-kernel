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
import Kernel.External.Payment.Juspay.Types as Reexport (CreateOrderResp (..), Currency (..), MandateFrequency (..), MandateStatus (..), MandateType (..), NotificationStatus (..), OfferListStatus (..), OfferStatus (..), PaymentLinks (..), PaymentStatus (..), TransactionStatus (..))
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
    mandateFrequency :: Maybe MandateFrequency,
    mandateStartDate :: Maybe Text,
    mandateEndDate :: Maybe Text
  }

newtype OrderStatusReq = OrderStatusReq
  { orderShortId :: Text
  }

data OrderStatusResp
  = OrderStatusResp
      { eventName :: Maybe PaymentStatus,
        orderShortId :: Text,
        transactionUUID :: Maybe Text,
        transactionStatusId :: Int,
        transactionStatus :: TransactionStatus,
        paymentMethodType :: Maybe Text,
        paymentMethod :: Maybe Text,
        respMessage :: Maybe Text,
        respCode :: Maybe Text,
        gatewayReferenceId :: Maybe Text,
        bankErrorMessage :: Maybe Text,
        bankErrorCode :: Maybe Text,
        amount :: HighPrecMoney,
        currency :: Currency,
        dateCreated :: Maybe UTCTime
      }
  | MandateOrderStatusResp
      { eventName :: Maybe PaymentStatus,
        orderShortId :: Text,
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
        bankErrorMessage :: Maybe Text,
        bankErrorCode :: Maybe Text,
        mandateStartDate :: Maybe UTCTime,
        mandateEndDate :: Maybe UTCTime,
        mandateId :: Text,
        mandateStatus :: MandateStatus,
        mandateFrequency :: MandateFrequency,
        mandateMaxAmount :: HighPrecMoney,
        payerVpa :: Maybe Text,
        upi :: Maybe Upi
      }
  | MandateStatusResp
      { eventName :: Maybe PaymentStatus,
        orderShortId :: Text,
        status :: MandateStatus,
        mandateStartDate :: Maybe UTCTime,
        mandateEndDate :: Maybe UTCTime,
        mandateId :: Text,
        mandateFrequency :: MandateFrequency,
        mandateMaxAmount :: HighPrecMoney,
        upi :: Maybe Upi
      }
  | PDNNotificationStatusResp
      { eventName :: Maybe PaymentStatus,
        notificationStatus :: NotificationStatus,
        sourceObject :: Maybe Text,
        endDate :: Text,
        sourceInfo :: SourceInfo,
        notificationType :: Maybe Text,
        juspayProviedId :: Text,
        notificationId :: Text
      }
  | BadStatusResp
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Upi = Upi
  { payerApp :: Maybe Text,
    payerVpa :: Maybe Text,
    payerAppName :: Maybe Text,
    txnFlowType :: Maybe Text
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- notification request --
data MandateNotificationReq = MandateNotificationReq
  { amount :: HighPrecMoney,
    txnDate :: UTCTime,
    mandateId :: Text,
    notificationId :: Text,
    description :: Text
  }
  deriving (Eq, Show, Generic)

data MandateNotificationRes = MandateNotificationRes
  { juspayProvidedId :: Text,
    sourceInfo :: SourceInfo,
    notificationId :: Text,
    providerName :: Maybe Text,
    notificationType :: Maybe Text,
    description :: Text,
    status :: NotificationStatus,
    dateCreated :: Maybe UTCTime,
    lastUpdated :: Maybe UTCTime
  }

data SourceInfo = SourceInfo
  { sourceAmount :: Maybe HighPrecMoney,
    txnDate :: Maybe UTCTime
  }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

--- Notification status response and request --
newtype NotificationStatusReq = NotificationStatusReq
  {notificationId :: Text}

data NotificationStatusResp = NotificationStatusResp
  { id :: Text,
    sourceObject :: Maybe Text,
    sourceObjectId :: Maybe Text, -- mandate Id in this case --
    objectReferenceId :: Text,
    providerName :: Maybe Text,
    notificationType :: Maybe Text,
    sourceInfo :: SourceInfo,
    providerResponse :: Maybe ProviderResponse,
    description :: Text,
    status :: NotificationStatus,
    dateCreated :: Maybe UTCTime,
    lastUpdated :: Maybe UTCTime
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data ProviderResponse = ProviderResponse
  { providerRefId :: Maybe Text,
    notificationDate :: Maybe UTCTime
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- mandate pause | resume | revoke request --

data MandateCommandsReq = Pause MandatePauseReq | Resume MandateResumeReq | Revoke MandateRevokeReq

data MandatePauseReq = MandatePauseReq {mandateId :: Text, pauseStartDate :: UTCTime, pauseEndDate :: Maybe UTCTime}

data MandateResumeReq = MandateResumeReq {mandateId :: Text, resumeDate :: UTCTime}

newtype MandateRevokeReq = MandateRevokeReq {mandateId :: Text}

type MandateRevokeRes = APISuccess

--- mandate Execution request and response ---

data MandateExecutionReq = MandateExecutionReq
  { notificationId :: Text,
    orderId :: Text,
    amount :: HighPrecMoney,
    customerId :: Text,
    mandateId :: Text,
    executionDate :: UTCTime
  }

data MandateExecutionRes = MandateExecutionRes
  { orderId :: Text,
    txnId :: Text,
    txnUUID :: Text,
    status :: TransactionStatus
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- offer list request --

data OfferListReq = OfferListReq
  { order :: OfferOrder,
    customer :: Maybe OfferCustomer,
    planId :: Text,
    registrationDate :: UTCTime,
    dutyDate :: UTCTime,
    paymentMode :: Text
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
  { txnId :: Text,
    offers :: [Text],
    customerId :: Text,
    amount :: HighPrecMoney,
    currency :: Currency,
    planId :: Text,
    registrationDate :: UTCTime,
    dutyDate :: UTCTime,
    paymentMode :: Text
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

--- auto refund request ---
data AutoRefundReq = AutoRefundReq
  { orderId :: Text,
    requestId :: Text,
    amount :: Double
  }
