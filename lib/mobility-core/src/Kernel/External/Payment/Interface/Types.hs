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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Kernel.External.Payment.Interface.Types
  ( module Kernel.External.Payment.Interface.Types,
    module Reexport,
  )
where

import Control.Lens
import Data.Aeson.Types
import Data.OpenApi hiding (description, email, name, title)
import Data.Time
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import qualified Kernel.External.Payment.Juspay.Config as Juspay
import Kernel.External.Payment.Juspay.Types as Reexport (CreateOrderResp (..), MandateFrequency (..), MandateStatus (..), MandateType (..), NotificationStatus (..), OfferListStatus (..), OfferState (..), OfferStatus (..), PaymentLinks (..), PaymentStatus (..), TransactionStatus (..))
import qualified Kernel.External.Payment.Stripe.Config as Stripe
import Kernel.External.Payment.Stripe.Types as Reexport hiding (RefundStatus (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common

castToTransactionStatus :: PaymentIntentStatus -> TransactionStatus
castToTransactionStatus Succeeded = CHARGED
castToTransactionStatus Cancelled = CANCELLED
castToTransactionStatus Processing = AUTHORIZING
castToTransactionStatus RequiresAction = PENDING_VBV
castToTransactionStatus RequiresCapture = STARTED
castToTransactionStatus RequiresConfirmation = NEW
castToTransactionStatus RequiresPaymentMethod = NEW

caseToPaymentIntentStatus :: TransactionStatus -> PaymentIntentStatus
caseToPaymentIntentStatus CHARGED = Succeeded
caseToPaymentIntentStatus CANCELLED = Cancelled
caseToPaymentIntentStatus AUTHORIZING = Processing
caseToPaymentIntentStatus PENDING_VBV = RequiresAction
caseToPaymentIntentStatus STARTED = RequiresCapture
caseToPaymentIntentStatus NEW = RequiresConfirmation
caseToPaymentIntentStatus AUTHENTICATION_FAILED = Cancelled
caseToPaymentIntentStatus AUTHORIZATION_FAILED = Cancelled
caseToPaymentIntentStatus JUSPAY_DECLINED = Cancelled
caseToPaymentIntentStatus _ = RequiresPaymentMethod

castChargeToTransactionStatus :: ChargeStatus -> TransactionStatus
castChargeToTransactionStatus CHARGE_SUCCEEDED = CHARGED
castChargeToTransactionStatus CHARGE_PENDING = PENDING_VBV
castChargeToTransactionStatus CHARGE_FAILED = AUTHORIZATION_FAILED
castChargeToTransactionStatus CHARGE_REFUNDED = AUTO_REFUNDED
castChargeToTransactionStatus CHARGE_DISPUTED = CANCELLED -- does not have correct status for dispute yet
castChargeToTransactionStatus CHARGE_UNCAPTURED = STARTED
castChargeToTransactionStatus CHARGE_CANCELED = CANCELLED

data PaymentServiceConfig = JuspayConfig Juspay.JuspayCfg | StripeConfig Stripe.StripeCfg
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
    mandateEndDate :: Maybe Text,
    metadataGatewayReferenceId :: Maybe Text,
    optionsGetUpiDeepLinks :: Maybe Bool,
    metadataExpiryInMins :: Maybe Int,
    splitSettlementDetails :: Maybe SplitSettlementDetails,
    basket :: Maybe [Basket]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Basket = Basket
  { id :: Text,
    unitPrice :: HighPrecMoney,
    quantity :: Int
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Split = Split
  { amount :: HighPrecMoney,
    merchantCommission :: HighPrecMoney,
    subMid :: Text,
    uniqueSplitId :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

newtype Vendor = Vendor
  { split :: [Split]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data SplitSettlementDetails
  = AmountBased SplitSettlementDetailsAmount
  | PercentageBased SplitSettlementDetailsPercentage
  deriving (Show, Eq, Generic, ToSchema, ToJSON, FromJSON)

data SplitSettlementDetailsAmount = SplitSettlementDetailsAmount
  { marketplace :: Marketplace,
    mdrBorneBy :: MBY,
    vendor :: Vendor
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data RefundSplitSettlementDetails = RefundSplitSettlementDetails
  { marketplace :: RefundMarketplace,
    mdrBorneBy :: MBY,
    vendor :: RefundVendor
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

newtype RefundVendor = RefundVendor
  { split :: [RefundSplit]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data RefundSplit = RefundSplit
  { refundAmount :: HighPrecMoney,
    subMid :: Text,
    uniqueSplitId :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data SplitSettlementDetailsPercentage = SplitSettlementDetailsPercentage
  { marketplace :: MarketplacePercentage,
    mdrBorneBy :: MBY,
    vendor :: VendorPercentage
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data MBY = MARKETPLACE | VENDOR | ALL deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)

newtype Marketplace = Marketplace
  { amount :: HighPrecMoney
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

newtype RefundMarketplace = RefundMarketplace
  { refundAmount :: HighPrecMoney
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

newtype MarketplacePercentage = MarketplacePercentage
  { amountPercentage :: Double
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

newtype VendorPercentage = VendorPercentage
  { split :: [SplitPercentage]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data SplitPercentage = SplitPercentage
  { amountPercentage :: Double,
    merchantCommissionPercentage :: Double,
    subMid :: Text,
    uniqueSplitId :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

newtype OrderStatusReq = OrderStatusReq
  { orderShortId :: Text
  }

data Offer = Offer
  { offerId :: Maybe Text,
    offerCode :: Maybe Text,
    status :: OfferState
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data OrderStatusResp
  = OrderStatusResp
      { eventName :: Maybe PaymentStatus,
        orderShortId :: Text,
        transactionUUID :: Maybe Text,
        txnId :: Maybe Text,
        transactionStatusId :: Int,
        transactionStatus :: TransactionStatus,
        paymentMethodType :: Maybe Text,
        paymentMethod :: Maybe Text,
        paymentGatewayResponse :: Maybe PaymentGatewayResponse,
        respMessage :: Maybe Text,
        respCode :: Maybe Text,
        gatewayReferenceId :: Maybe Text,
        bankErrorMessage :: Maybe Text,
        bankErrorCode :: Maybe Text,
        amount :: HighPrecMoney,
        currency :: Currency,
        dateCreated :: Maybe UTCTime,
        isRetriedOrder :: Maybe Bool,
        isRetargetedOrder :: Maybe Bool,
        retargetPaymentLink :: Maybe Text,
        retargetPaymentLinkExpiry :: Maybe UTCTime,
        amountRefunded :: Maybe HighPrecMoney,
        refunds :: [RefundsData],
        payerVpa :: Maybe Text,
        upi :: Maybe Upi,
        card :: Maybe CardInfo,
        splitSettlementResponse :: Maybe SplitSettlementResponse,
        effectiveAmount :: Maybe HighPrecMoney,
        offers :: Maybe [Offer]
      }
  | MandateOrderStatusResp
      { eventName :: Maybe PaymentStatus,
        orderShortId :: Text,
        transactionUUID :: Maybe Text,
        txnId :: Maybe Text,
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
        upi :: Maybe Upi,
        amountRefunded :: Maybe HighPrecMoney,
        refunds :: [RefundsData]
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
        sourceInfo :: SourceInfo,
        notificationType :: Maybe Text,
        juspayProviedId :: Text,
        notificationId :: Text,
        responseCode :: Maybe Text,
        responseMessage :: Maybe Text
      }
  | BadStatusResp
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data SplitSettlementResponse = SplitSettlementResponse
  { splitDetails :: Maybe [SplitDetailsResponse],
    splitApplied :: Maybe Bool
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data SplitDetailsResponse = SplitDetailsResponse
  { subVendorId :: Maybe Text,
    amount :: Maybe HighPrecMoney,
    merchantCommission :: Maybe HighPrecMoney,
    gatewaySubAccountId :: Maybe Text,
    epgTxnId :: Maybe Text,
    uniqueSplitId :: Maybe Text
  }
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

data CardInfo = CardInfo
  { lastFourDigits :: Maybe Text,
    cardType :: Maybe Text
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

data PaymentGatewayResponse = PaymentGatewayResponse
  { txnId :: Maybe Text,
    rrn :: Maybe Text,
    respMessage :: Maybe Text,
    respCode :: Maybe Text,
    epgTxnId :: Maybe Text,
    created :: Maybe UTCTime,
    authIdCode :: Maybe Text
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
    notificationDate :: Maybe UTCTime,
    responseCode :: Maybe Text,
    responseMessage :: Maybe Text
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
    executionDate :: UTCTime,
    splitSettlementDetails :: Maybe SplitSettlementDetailsAmount
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
    paymentMode :: Text,
    numOfRides :: Int,
    offerListingMetric :: Maybe UDF6
  }

data UDF6 = IS_VISIBLE | IS_APPLICABLE | LIST_BASED_ON_DATE UTCTime
  deriving stock (Show, Eq, Generic, Read)

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
  deriving (Generic, Show, FromJSON, ToJSON)
  deriving anyclass (ToSchema)

data BestOfferCombination = BestOfferCombination
  { offers :: [BestOfferCombinationOffer],
    orderBreakup :: OrderBreakup
  }
  deriving (Generic, Show, FromJSON, ToJSON)
  deriving anyclass (ToSchema)

data BestOfferCombinationOffer = BestOfferCombinationOffer
  { offerId :: Text,
    cashbackAmount :: HighPrecMoney,
    discountAmount :: HighPrecMoney,
    merchantDiscountAmount :: HighPrecMoney,
    totalOfferedAmount :: HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON)
  deriving anyclass (ToSchema)

data OrderBreakup = OrderBreakup
  { orderAmount :: HighPrecMoney,
    finalOrderAmount :: HighPrecMoney,
    discountAmount :: HighPrecMoney,
    merchantDiscountAmount :: HighPrecMoney,
    cashbackAmount :: HighPrecMoney,
    offerAmount :: HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON)
  deriving anyclass (ToSchema)

data OfferResp = OfferResp
  { offerId :: Text,
    status :: OfferListStatus,
    offerDescription :: OfferDescription,
    orderAmount :: HighPrecMoney,
    finalOrderAmount :: HighPrecMoney,
    discountAmount :: HighPrecMoney,
    offerCode :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
  deriving anyclass (ToSchema)

data OfferDescription = OfferDescription
  { sponsoredBy :: Maybe Text,
    title :: Maybe Text,
    description :: Maybe Text,
    tnc :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
  deriving anyclass (ToSchema)

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
    paymentMode :: Text,
    numOfRides :: Int
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
    amount :: HighPrecMoney,
    splitSettlementDetails :: Maybe RefundSplitSettlementDetails
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AutoRefundResp = AutoRefundResp
  { orderId :: Text,
    merchantId :: Text,
    customerId :: Text,
    currency :: Currency,
    amountRefunded :: Double,
    refunds :: [RefundsData]
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data RefundStatus = REFUND_PENDING | REFUND_FAILURE | REFUND_SUCCESS | MANUAL_REVIEW | REFUND_CANCELED | REFUND_REQUIRES_ACTION
  deriving stock (Show, Eq, Read, Ord, Generic)

instance FromJSON RefundStatus where
  parseJSON (String "FAILURE") = pure REFUND_FAILURE
  parseJSON (String "PENDING") = pure REFUND_PENDING
  parseJSON (String "SUCCESS") = pure REFUND_SUCCESS
  parseJSON (String "MANUAL_REVIEW") = pure MANUAL_REVIEW
  parseJSON (String "REFUND_CANCELED") = pure REFUND_CANCELED
  parseJSON (String "REFUND_REQUIRES_ACTION") = pure REFUND_REQUIRES_ACTION
  parseJSON (String _) = parseFail "Expected type"
  parseJSON e = typeMismatch "String" e

instance ToJSON RefundStatus where
  toJSON REFUND_FAILURE = "FAILURE"
  toJSON REFUND_PENDING = "PENDING"
  toJSON REFUND_SUCCESS = "SUCCESS"
  toJSON MANUAL_REVIEW = "MANUAL_REVIEW"
  toJSON REFUND_CANCELED = "REFUND_CANCELED"
  toJSON REFUND_REQUIRES_ACTION = "REFUND_REQUIRES_ACTION"

instance ToSchema RefundStatus where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "RefundStatus") $
        mempty
          & type_ ?~ OpenApiString
          & enum_ ?~ (map toJSON [REFUND_PENDING, REFUND_FAILURE, REFUND_SUCCESS, MANUAL_REVIEW, REFUND_CANCELED, REFUND_REQUIRES_ACTION])

data RefundsData = RefundsData
  { idAssignedByServiceProvider :: Maybe Text,
    amount :: HighPrecMoney,
    status :: RefundStatus,
    errorMessage :: Maybe Text,
    errorCode :: Maybe Text,
    initiatedBy :: Maybe Text,
    requestId :: Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Request to create a Connect Account
data CreateConnectAccountReq = CreateConnectAccountReq
  { country :: Context.Country,
    email :: Maybe Text,
    mobileNumber :: Text,
    dateOfBirth :: Day,
    firstName :: Text,
    lastName :: Maybe Text,
    ssnLast4 :: Maybe Text,
    idNumber :: Maybe Text,
    address :: Maybe Address,
    businessType :: BusinessType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreateConnectAccountResp = CreateConnectAccountResp
  { accountId :: AccountId,
    accountUrl :: Text,
    accountUrlExpiry :: UTCTime,
    chargesEnabled :: Bool,
    detailsSubmitted :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data RetryAccountLink = RetryAccountLink
  { accountId :: AccountId,
    accountUrl :: Text,
    accountUrlExpiry :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data ConnectAccountResp = ConnectAccountResp
  { accountId :: AccountId,
    chargesEnabled :: Bool,
    detailsSubmitted :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreateCustomerReq = CreateCustomerReq
  { email :: Maybe Text,
    name :: Maybe Text,
    lastName :: Maybe Text,
    phone :: Maybe Text,
    objectReferenceId :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    optionsGetClientAuthToken :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreateCustomerResp = CreateCustomerResp
  { customerId :: CustomerId,
    clientAuthToken :: Maybe Text,
    clientAuthTokenExpiry :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data OrderUpdateReq = OrderUpdateReq
  { amount :: HighPrecMoney,
    orderShortId :: Text,
    splitSettlementDetails :: Maybe SplitSettlementDetailsAmount
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data OrderUpdateResp = OrderUpdateResp
  { orderId :: Maybe Text,
    amount :: Maybe HighPrecMoney
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreatePaymentIntentReq = CreatePaymentIntentReq
  { orderShortId :: Text,
    amount :: HighPrecMoney,
    applicationFeeAmount :: HighPrecMoney,
    currency :: Currency,
    customer :: CustomerId,
    paymentMethod :: PaymentMethodId,
    receiptEmail :: Maybe Text,
    driverAccountId :: AccountId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreatePaymentIntentResp = CreatePaymentIntentResp
  { paymentIntentId :: PaymentIntentId,
    clientSecret :: Text,
    status :: PaymentIntentStatus
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreateSetupIntentResp = CreateSetupIntentResp
  { setupIntentId :: SetupIntentId,
    clientSecret :: Text,
    status :: PaymentIntentStatus
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CustomerCard = CustomerCard
  { cardId :: PaymentMethodId,
    brand :: Text,
    last4 :: Text,
    expMonth :: Int,
    expYear :: Int,
    country :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

type CustomerCardListResp = [CustomerCard]

data VerifyVPAReq = VerifyVPAReq
  { vpa :: Text,
    customerId :: Maybe Text,
    orderId :: Maybe Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data VerifyVPAResp = VerifyVPAResp
  { vpa :: Text,
    status :: Text,
    customerName :: Maybe Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreateRefundReq = CreateRefundReq
  { orderShortId :: Text,
    orderId :: Text,
    refundsId :: Text,
    paymentIntentId :: PaymentIntentId,
    amount :: Maybe HighPrecMoney,
    refundApplicationFee :: Bool,
    driverAccountId :: AccountId,
    email :: Maybe Text
  }

data CreateRefundResp = CreateRefundResp
  { id :: RefundId,
    status :: RefundStatus,
    reverseTransferId :: Maybe Text,
    errorCode :: Maybe Text
  }

data GetRefundReq = GetRefundReq
  { id :: RefundId,
    driverAccountId :: AccountId
  }

data GetRefundResp = GetRefundResp
  { id :: RefundId,
    orderShortId :: Maybe Text,
    orderId :: Maybe Text,
    refundsId :: Maybe Text,
    paymentIntentId :: Maybe PaymentIntentId,
    amount :: HighPrecMoney,
    currency :: Currency,
    status :: RefundStatus,
    reverseTransferId :: Maybe Text,
    errorCode :: Maybe Text
  }
  deriving stock (Show)

type CancelRefundReq = GetRefundReq

type CancelRefundResp = GetRefundResp

derivePersistField "RefundStatus"

$(mkBeamInstancesForEnum ''RefundStatus)
