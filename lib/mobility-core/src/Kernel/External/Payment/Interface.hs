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

import qualified Kernel.External.Payment.Interface.Juspay as Juspay
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
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  CreateOrderReq ->
  m CreateOrderResp
createOrder serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> do
    let req' = req {metadataGatewayReferenceId = cfg.gatewayReferenceId}
    Juspay.createOrder cfg req'
  StripeConfig _ -> throwError $ InternalError "Stripe Create Order not supported."

orderStatus ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  OrderStatusReq ->
  m OrderStatusResp
orderStatus serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.orderStatus cfg req
  StripeConfig _ -> throwError $ InternalError "Stripe Order Status not supported."

offerList ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  OfferListReq ->
  m OfferListResp
offerList serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.offerList cfg req
  StripeConfig _ -> throwError $ InternalError "Stripe Offer List not supported."

offerApply ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  OfferApplyReq ->
  m OfferApplyResp
offerApply serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.offerApply cfg req
  StripeConfig _ -> throwError $ InternalError "Stripe Offer Apply not supported."

offerNotify ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  OfferNotifyReq ->
  m OfferNotifyResp
offerNotify serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.offerNotify cfg req
  StripeConfig _ -> throwError $ InternalError "Stripe Offer Notify not supported."

mandateRevoke ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  MandateRevokeReq ->
  m MandateRevokeRes
mandateRevoke serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.mandateRevoke cfg req
  StripeConfig _ -> throwError $ InternalError "Stripe Mandate Revoke not supported."

mandateNotification ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  MandateNotificationReq ->
  m MandateNotificationRes
mandateNotification serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.mandateNotification cfg req
  StripeConfig _ -> throwError $ InternalError "Stripe Mandate Notification not supported."

mandateNotificationStatus ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  NotificationStatusReq ->
  m NotificationStatusResp
mandateNotificationStatus serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.mandateNotificationStatus cfg req
  StripeConfig _ -> throwError $ InternalError "Stripe Mandate Notification Status not supported."

mandateExecution ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  MandateExecutionReq ->
  m MandateExecutionRes
mandateExecution serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.mandateExecution cfg req
  StripeConfig _ -> throwError $ InternalError "Stripe Mandate Execution not supported."

autoRefunds ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  AutoRefundReq ->
  m AutoRefundResp
autoRefunds serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.autoRefund cfg req
  StripeConfig _ -> throwError $ InternalError "Stripe Auto Refunds not supported."

createIndividualConnectAccount ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  IndividualConnectAccountReq ->
  m IndividualConnectAccountResp
createIndividualConnectAccount serviceConfig req = case serviceConfig of
  JuspayConfig _ -> throwError $ InternalError "Juspay Create Individual Connect Account not supported."
  StripeConfig cfg -> Stripe.createIndividualConnectAccount cfg req

retryAccountLink ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  PaymentServiceConfig ->
  Stripe.AccountId ->
  m RetryAccountLink
retryAccountLink config accountId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Retry Account Link not supported."
  StripeConfig cfg -> Stripe.retryAccountLink cfg accountId

getAccount ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  PaymentServiceConfig ->
  Stripe.AccountId ->
  m ConnectAccountResp
getAccount config accountId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Get Account not supported."
  StripeConfig cfg -> Stripe.getAccount cfg accountId

createCustomer ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  PaymentServiceConfig ->
  CreateCustomerReq ->
  m CreateCustomerResp
createCustomer config req = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Create Customer not supported."
  StripeConfig cfg -> Stripe.createCustomer cfg req

createEphemeralKeys ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  PaymentServiceConfig ->
  CustomerId ->
  m Text
createEphemeralKeys config customerId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Create Ephemeral Keys not supported."
  StripeConfig cfg -> Stripe.createEphemeralKeys cfg customerId

deleteCard ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  PaymentServiceConfig ->
  PaymentMethodId ->
  m ()
deleteCard config cardId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Delete Card not supported."
  StripeConfig cfg -> Stripe.deleteCard cfg cardId

getCardList ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  PaymentServiceConfig ->
  CustomerId ->
  m CustomerCardListResp
getCardList config customerId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Get Card List not supported."
  StripeConfig cfg -> Stripe.getCardList cfg customerId

createPaymentIntent ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  PaymentServiceConfig ->
  CreatePaymentIntentReq ->
  m CreatePaymentIntentResp
createPaymentIntent config req = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Create Payment Intent not supported."
  StripeConfig cfg -> Stripe.createPaymentIntent cfg req

updatePaymentMethodInIntent ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  PaymentServiceConfig ->
  PaymentIntentId ->
  PaymentMethodId ->
  m ()
updatePaymentMethodInIntent config paymentIntentId paymentMethodId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Update Payment Method In Intent not supported."
  StripeConfig cfg -> Stripe.updatePaymentMethodInIntent cfg paymentIntentId paymentMethodId

getPaymentIntent ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  PaymentServiceConfig ->
  PaymentIntentId ->
  m CreatePaymentIntentResp
getPaymentIntent config paymentIntentId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Get Payment Intent not supported."
  StripeConfig cfg -> Stripe.getPaymentIntent cfg paymentIntentId

capturePaymentIntent ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  PaymentServiceConfig ->
  PaymentIntentId ->
  HighPrecMoney ->
  HighPrecMoney ->
  m ()
capturePaymentIntent config paymentIntentId amount applicationFeeAmount = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Capture Payment Intent not supported."
  StripeConfig cfg -> Stripe.capturePaymentIntent cfg paymentIntentId amount applicationFeeAmount

updateAmountInPaymentIntent ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  PaymentServiceConfig ->
  PaymentIntentId ->
  HighPrecMoney ->
  HighPrecMoney ->
  m ()
updateAmountInPaymentIntent config paymentIntentId amount applicationFeeAmount = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Update Amount In Payment Intent not supported."
  StripeConfig cfg -> Stripe.updateAmountInPaymentIntent cfg paymentIntentId amount applicationFeeAmount

createSetupIntent ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  PaymentServiceConfig ->
  CustomerId ->
  m CreateSetupIntentResp
createSetupIntent config customerId = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Create Setup Intent not supported."
  StripeConfig cfg -> Stripe.createSetupIntent cfg customerId
