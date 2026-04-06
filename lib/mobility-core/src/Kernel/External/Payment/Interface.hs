{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.External.Payment.Interface
  ( module Reexport,
    module Kernel.External.Payment.Interface,
  )
where

import qualified Data.Aeson as A
import qualified Kernel.External.Payment.Interface.Juspay as Juspay
import qualified Kernel.External.Payment.Interface.PaytmEDC as PaytmEDC
import qualified Kernel.External.Payment.Interface.Stripe as Stripe
import Kernel.External.Payment.Interface.Types as Reexport
import qualified Kernel.External.Payment.Stripe.Types as Stripe
import Kernel.External.Payment.Types as Reexport
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common

createOrder ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Maybe Text ->
  CreateOrderReq ->
  m CreateOrderResp
createOrder serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> do
    let req' = (req :: CreateOrderReq) {metadataGatewayReferenceId = cfg.gatewayReferenceId}
    Juspay.createOrder cfg mRoutingId req'
  StripeConfig _ -> throwError $ InternalError "Stripe Create Order not supported."
  PaytmEDCConfig cfg -> PaytmEDC.createOrder cfg mRoutingId req

orderStatus ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Maybe Text ->
  OrderStatusReq ->
  m OrderStatusResp
orderStatus serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> Juspay.orderStatus cfg mRoutingId req
  StripeConfig _ -> throwError $ InternalError "Stripe Order Status not supported."
  PaytmEDCConfig cfg -> PaytmEDC.orderStatus cfg mRoutingId req

updateOrder ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Maybe Text ->
  OrderUpdateReq ->
  m OrderUpdateResp
updateOrder serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> Juspay.updateOrder cfg mRoutingId req
  StripeConfig _ -> throwError $ InternalError "Stripe Update Order not supported."
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Update Order not supported."

offerList ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Maybe Text ->
  OfferListReq ->
  m OfferListResp
offerList serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> Juspay.offerList cfg mRoutingId req
  StripeConfig _ -> throwError $ InternalError "Stripe Offer List not supported."
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Offer List not supported."

offerApply ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Maybe Text ->
  OfferApplyReq ->
  m OfferApplyResp
offerApply serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> Juspay.offerApply cfg mRoutingId req
  StripeConfig _ -> throwError $ InternalError "Stripe Offer Apply not supported."
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Offer Apply not supported."

offerNotify ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Maybe Text ->
  OfferNotifyReq ->
  m OfferNotifyResp
offerNotify serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> Juspay.offerNotify cfg mRoutingId req
  StripeConfig _ -> throwError $ InternalError "Stripe Offer Notify not supported."
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Offer Notify not supported."

mandateRevoke ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Maybe Text ->
  MandateRevokeReq ->
  m MandateRevokeRes
mandateRevoke serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> Juspay.mandateRevoke cfg mRoutingId req
  StripeConfig _ -> throwError $ InternalError "Stripe Mandate Revoke not supported."
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Mandate Revoke not supported."

mandateNotification ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Maybe Text ->
  MandateNotificationReq ->
  m MandateNotificationRes
mandateNotification serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> Juspay.mandateNotification cfg mRoutingId req
  StripeConfig _ -> throwError $ InternalError "Stripe Mandate Notification not supported."
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Mandate Notification not supported."

mandateNotificationStatus ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Maybe Text ->
  NotificationStatusReq ->
  m NotificationStatusResp
mandateNotificationStatus serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> Juspay.mandateNotificationStatus cfg mRoutingId req
  StripeConfig _ -> throwError $ InternalError "Stripe Mandate Notification Status not supported."
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Mandate Notification Status not supported."

mandateExecution ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Maybe Text ->
  MandateExecutionReq ->
  m MandateExecutionRes
mandateExecution serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> Juspay.mandateExecution cfg mRoutingId req
  StripeConfig _ -> throwError $ InternalError "Stripe Mandate Execution not supported."
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Mandate Execution not supported."

autoRefunds ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Maybe Text ->
  AutoRefundReq ->
  m AutoRefundResp
autoRefunds serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> Juspay.autoRefund cfg mRoutingId req
  StripeConfig _ -> throwError $ InternalError "Stripe Auto Refunds not supported."
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Auto Refunds not supported."

createIndividualConnectAccount ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  IndividualConnectAccountReq ->
  m IndividualConnectAccountResp
createIndividualConnectAccount serviceConfig req = case serviceConfig of
  JuspayConfig _ -> throwError $ InternalError "Juspay Create Individual Connect Account not supported."
  StripeConfig cfg -> Stripe.createIndividualConnectAccount cfg req
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Create Individual Connect Account not supported."

retryAccountLink ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Stripe.AccountId ->
  m RetryAccountLink
retryAccountLink config accountId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Retry Account Link not supported."
  StripeConfig cfg -> Stripe.retryAccountLink cfg accountId
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Retry Account Link not supported."

getAccount ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Stripe.AccountId ->
  m ConnectAccountResp
getAccount config accountId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Get Account not supported."
  StripeConfig cfg -> Stripe.getAccount cfg accountId
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Get Account not supported."

createCustomer ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  CreateCustomerReq ->
  m CreateCustomerResp
createCustomer config req = case config of
  JuspayConfig cfg -> Juspay.createCustomer cfg req
  StripeConfig cfg -> Stripe.createCustomer cfg req
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Create Customer not supported."

getCustomer ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  CustomerId ->
  m CreateCustomerResp
getCustomer config customerId = case config of
  JuspayConfig cfg -> Juspay.getCustomer cfg customerId
  StripeConfig cfg -> Stripe.getCustomer cfg customerId
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Get Customer not supported."

createEphemeralKeys ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  CustomerId ->
  m Text
createEphemeralKeys config customerId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Create Ephemeral Keys not supported."
  StripeConfig cfg -> Stripe.createEphemeralKeys cfg customerId
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Create Ephemeral Keys not supported."

deleteCard ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  PaymentMethodId ->
  m ()
deleteCard config cardId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Delete Card not supported."
  StripeConfig cfg -> Stripe.deleteCard cfg cardId
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Delete Card not supported."

getCardList ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  CustomerId ->
  m CustomerCardListResp
getCardList config customerId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Get Card List not supported."
  StripeConfig cfg -> Stripe.getCardList cfg customerId
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Get Card List not supported."

createPaymentIntent ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  CreatePaymentIntentReq ->
  m CreatePaymentIntentResp
createPaymentIntent config req = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Create Payment Intent not supported."
  StripeConfig cfg -> Stripe.createPaymentIntent cfg req
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Create Payment Intent not supported."

updatePaymentMethodInIntent ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  PaymentIntentId ->
  PaymentMethodId ->
  m ()
updatePaymentMethodInIntent config paymentIntentId paymentMethodId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Update Payment Method In Intent not supported."
  StripeConfig cfg -> Stripe.updatePaymentMethodInIntent cfg paymentIntentId paymentMethodId
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Update Payment Method In Intent not supported."

getPaymentIntent ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  PaymentIntentId ->
  m CreatePaymentIntentResp
getPaymentIntent config paymentIntentId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Get Payment Intent not supported."
  StripeConfig cfg -> Stripe.getPaymentIntent cfg paymentIntentId
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Get Payment Intent not supported."

capturePaymentIntent ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  PaymentIntentId ->
  HighPrecMoney ->
  HighPrecMoney ->
  m ()
capturePaymentIntent config paymentIntentId amount applicationFeeAmount = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Capture Payment Intent not supported."
  StripeConfig cfg -> Stripe.capturePaymentIntent cfg paymentIntentId amount applicationFeeAmount
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Capture Payment Intent not supported."

updateAmountInPaymentIntent ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  PaymentIntentId ->
  HighPrecMoney ->
  HighPrecMoney ->
  m ()
updateAmountInPaymentIntent config paymentIntentId amount applicationFeeAmount = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Update Amount In Payment Intent not supported."
  StripeConfig cfg -> Stripe.updateAmountInPaymentIntent cfg paymentIntentId amount applicationFeeAmount
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Update Amount In Payment Intent not supported."

createSetupIntent ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  CustomerId ->
  m CreateSetupIntentResp
createSetupIntent config customerId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Create Setup Intent not supported."
  StripeConfig cfg -> Stripe.createSetupIntent cfg customerId
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Create Setup Intent not supported."

getCard ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  PaymentMethodId ->
  CustomerId ->
  m CustomerCard
getCard config cardId customerId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Get Card not supported."
  StripeConfig cfg -> Stripe.getCard cfg cardId customerId
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Get Card not supported."

cancelPaymentIntent ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  PaymentIntentId ->
  m CreatePaymentIntentResp
cancelPaymentIntent config paymentIntentId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Cancel Payment Intent not supported."
  StripeConfig cfg -> Stripe.cancelPaymentIntent cfg paymentIntentId
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Cancel Payment Intent not supported."

createRefund ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  CreateRefundReq ->
  m CreateRefundResp
createRefund config paymentIntentId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Create Refund not supported."
  StripeConfig cfg -> Stripe.createRefund cfg paymentIntentId
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Create Refund not supported."

getRefund ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  GetRefundReq ->
  m GetRefundResp
getRefund config paymentIntentId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Get Refund not supported."
  StripeConfig cfg -> Stripe.getRefund cfg paymentIntentId
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Get Refund not supported."

cancelRefund ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  CancelRefundReq ->
  m CancelRefundResp
cancelRefund config paymentIntentId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Cancel Refund not supported."
  StripeConfig cfg -> Stripe.cancelRefund cfg paymentIntentId
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Cancel Refund not supported."

verifyVPA ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Maybe Text ->
  VerifyVPAReq ->
  m VerifyVPAResp
verifyVPA config mRoutingId req = case config of
  JuspayConfig cfg -> Juspay.verifyVPA cfg mRoutingId req
  StripeConfig _ -> throwError $ InternalError "Stripe Verify VPA not supported."
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Verify VPA not supported."

isSplitEnabled :: PaymentServiceConfig -> Bool
isSplitEnabled = \case
  JuspayConfig cfg -> fromMaybe False cfg.isSplitEnabled
  StripeConfig _ -> False
  PaytmEDCConfig _ -> False

isPercentageSplit :: PaymentServiceConfig -> Bool
isPercentageSplit = \case
  JuspayConfig cfg -> fromMaybe False cfg.isPercentageSplit
  StripeConfig _ -> False
  PaytmEDCConfig _ -> False

isRefundSplitEnabled :: PaymentServiceConfig -> Bool
isRefundSplitEnabled = \case
  JuspayConfig cfg -> fromMaybe False cfg.isRefundSplitEnabled
  StripeConfig _ -> False
  PaytmEDCConfig _ -> False

getGatewayReferenceId :: PaymentServiceConfig -> Maybe Text
getGatewayReferenceId = \case
  JuspayConfig cfg -> cfg.gatewayReferenceId
  StripeConfig _ -> Nothing
  PaytmEDCConfig _ -> Nothing

offerSKUConfig :: PaymentServiceConfig -> Maybe Text
offerSKUConfig = \case
  JuspayConfig cfg -> cfg.offerSKUConfig
  StripeConfig _ -> Nothing
  PaytmEDCConfig _ -> Nothing

getUseDomainOffers :: PaymentServiceConfig -> Bool
getUseDomainOffers = \case
  JuspayConfig cfg -> fromMaybe False cfg.useDomainOffers
  StripeConfig cfg -> fromMaybe False cfg.useDomainOffers
  PaytmEDCConfig _ -> False

-- | Unified payment creation. Routes to Juspay createOrder or Stripe createPaymentIntent
--   based on PaymentServiceConfig. Domain code calls this single function.
createPayment ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Maybe Text ->
  CreatePaymentReq ->
  m CreatePaymentResp
createPayment serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> do
    let juspayReq =
          CreateOrderReq
            { orderId = req.orderShortId, -- Juspay uses orderShortId as orderId in their system
              orderShortId = req.orderShortId,
              amount = req.amount,
              customerId = req.customerId,
              customerEmail = req.customerEmail,
              customerPhone = req.customerPhone,
              customerFirstName = req.customerFirstName,
              customerLastName = req.customerLastName,
              createMandate = req.createMandate,
              mandateMaxAmount = req.mandateMaxAmount,
              mandateFrequency = req.mandateFrequency,
              mandateStartDate = req.mandateStartDate,
              mandateEndDate = req.mandateEndDate,
              metadataGatewayReferenceId = cfg.gatewayReferenceId,
              optionsGetUpiDeepLinks = req.optionsGetUpiDeepLinks,
              metadataExpiryInMins = req.metadataExpiryInMins,
              splitSettlementDetails = req.splitSettlementDetails,
              basket = req.basket
            }
    resp <- Juspay.createOrder cfg mRoutingId juspayReq
    let clientSecret' = maybe "" (\p -> p.payload.clientAuthToken) (Just resp.sdk_payload)
    pure
      CreatePaymentResp
        { paymentServiceOrderId = resp.id,
          clientSecret = clientSecret',
          status = resp.status,
          sdkPayload = Just (A.toJSON resp.sdk_payload),
          paymentLinks = resp.payment_links
        }
  StripeConfig cfg -> do
    paymentMethod <- req.paymentMethodId & fromMaybeM (InternalError "paymentMethodId required for Stripe")
    driverAccount <- req.driverAccountId & fromMaybeM (InternalError "driverAccountId required for Stripe")
    let stripeReq =
          CreatePaymentIntentReq
            { orderShortId = req.orderShortId,
              amount = req.amount,
              applicationFeeAmount = fromMaybe 0 req.applicationFeeAmount,
              currency = req.currency,
              customer = req.customerId,
              paymentMethod = paymentMethod,
              receiptEmail = req.receiptEmail,
              driverAccountId = driverAccount
            }
    resp <- Stripe.createPaymentIntent cfg stripeReq
    pure
      CreatePaymentResp
        { paymentServiceOrderId = resp.paymentIntentId,
          clientSecret = resp.clientSecret,
          status = castToTransactionStatus resp.status,
          sdkPayload = Nothing,
          paymentLinks = Nothing
        }
  PaytmEDCConfig cfg -> do
    let paytmReq =
          CreateOrderReq
            { orderId = req.orderShortId,
              orderShortId = req.orderShortId,
              amount = req.amount,
              customerId = req.customerId,
              customerEmail = req.customerEmail,
              customerPhone = req.customerPhone,
              customerFirstName = req.customerFirstName,
              customerLastName = req.customerLastName,
              createMandate = req.createMandate,
              mandateMaxAmount = req.mandateMaxAmount,
              mandateFrequency = req.mandateFrequency,
              mandateStartDate = req.mandateStartDate,
              mandateEndDate = req.mandateEndDate,
              metadataGatewayReferenceId = req.metadataGatewayReferenceId,
              optionsGetUpiDeepLinks = req.optionsGetUpiDeepLinks,
              metadataExpiryInMins = req.metadataExpiryInMins,
              splitSettlementDetails = req.splitSettlementDetails,
              basket = req.basket
            }
    resp <- PaytmEDC.createOrder cfg mRoutingId paytmReq
    let clientSecret' = maybe "" (\p -> p.payload.clientAuthToken) (Just resp.sdk_payload)
    pure
      CreatePaymentResp
        { paymentServiceOrderId = resp.id,
          clientSecret = clientSecret',
          status = resp.status,
          sdkPayload = Just (A.toJSON resp.sdk_payload),
          paymentLinks = resp.payment_links
        }

-- | Unified refund creation. Routes to Juspay autoRefunds or Stripe createRefund
--   based on PaymentServiceConfig.
refundPayment ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  Maybe Text ->
  RefundPaymentReq ->
  m RefundPaymentResp
refundPayment serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> do
    let juspayReq =
          AutoRefundReq
            { orderId = req.orderShortId,
              requestId = req.refundsId,
              amount = fromMaybe 0 req.amount,
              splitSettlementDetails = req.splitSettlementDetails
            }
    resp <- Juspay.autoRefund cfg mRoutingId juspayReq
    let firstRefund = listToMaybe resp.refunds
    pure
      RefundPaymentResp
        { refundId = req.refundsId,
          status = maybe REFUND_PENDING (.status) firstRefund,
          errorCode = firstRefund >>= (.errorCode),
          errorMessage = firstRefund >>= (.errorMessage)
        }
  StripeConfig cfg -> do
    paymentIntentId' <- req.paymentIntentId & fromMaybeM (InternalError "paymentIntentId required for Stripe refund")
    driverAccount <- req.driverAccountId & fromMaybeM (InternalError "driverAccountId required for Stripe refund")
    let stripeReq =
          CreateRefundReq
            { orderShortId = req.orderShortId,
              orderId = req.orderId,
              refundsId = req.refundsId,
              paymentIntentId = paymentIntentId',
              amount = req.amount,
              refundApplicationFee = False,
              driverAccountId = driverAccount,
              email = req.email
            }
    resp <- Stripe.createRefund cfg stripeReq
    pure
      RefundPaymentResp
        { refundId = resp.id.getRefundId,
          status = resp.status,
          errorCode = resp.errorCode,
          errorMessage = Nothing
        }
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC Refund not supported."

-- | Unified refund status check. For Stripe: calls GET /v1/refunds/:id.
--   For Juspay: refund status comes via orderStatus webhook — not a separate call.
getRefundStatus ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  GetRefundReq ->
  m RefundPaymentResp
getRefundStatus serviceConfig req = case serviceConfig of
  JuspayConfig _ -> throwError $ InternalError "Juspay getRefundStatus not supported — refund status comes via orderStatus webhook."
  StripeConfig cfg -> do
    resp <- Stripe.getRefund cfg req
    pure
      RefundPaymentResp
        { refundId = resp.id.getRefundId,
          status = resp.status,
          errorCode = resp.errorCode,
          errorMessage = Nothing
        }
  PaytmEDCConfig _ -> throwError $ InternalError "PaytmEDC getRefundStatus not supported."
