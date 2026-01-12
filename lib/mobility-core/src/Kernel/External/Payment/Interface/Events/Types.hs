{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Interface.Events.Types where

import Kernel.External.Payment.Interface.Types (GetRefundResp)
import qualified Kernel.External.Payment.Stripe.Types.Common as Stripe
import qualified Kernel.External.Payment.Stripe.Types.Webhook as Stripe
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data ServiceEventResp = ServiceEventResp
  { id :: Id Stripe.Event,
    apiVersion :: Text,
    createdAt :: UTCTime,
    eventData :: EventObject,
    livemode :: Bool,
    pendingWebhooks :: Integer,
    eventType :: Stripe.EventType
  }
  deriving stock (Show)

data EventObject
  = PaymentIntentSucceededEvent PaymentIntent
  | PaymentIntentPaymentFailedEvent PaymentIntent
  | PaymentIntentProcessingEvent PaymentIntent
  | PaymentIntentCanceledEvent PaymentIntent
  | PaymentIntentCreatedEvent PaymentIntent
  | PaymentIntentRequiresActionEvent PaymentIntent
  | SetupIntentSucceededEvent SetupIntent
  | SetupIntentSetupFailedEvent SetupIntent
  | SetupIntentCanceledEvent SetupIntent
  | SetupIntentCreatedEvent SetupIntent
  | SetupIntentRequiresActionEvent SetupIntent
  | ChargeSucceededEvent Charge
  | ChargeFailedEvent Charge
  | ChargeRefundedEvent Charge
  | ChargeDisputeCreatedEvent Charge
  | ChargeDisputeClosedEvent Charge
  | ChargeRefundUpdatedEvent Refund
  | RefundCreatedEvent Refund
  | RefundUpdatedEvent Refund
  | RefundFailedEvent Refund
  | CustomEvent Text
  deriving stock (Show)

--- SetupIntent Object ---
data SetupIntent = SetupIntent
  { id :: Text,
    application :: Maybe Text,
    automaticPaymentMethods :: Maybe AutomaticPaymentMethods,
    cancellationReason :: Maybe Text,
    clientSecret :: Maybe Text,
    createdAt :: UTCTime,
    customer :: Maybe Stripe.CustomerId,
    description :: Maybe Text,
    flowDirections :: Maybe [Text],
    lastSetupError :: Maybe Value,
    latestAttempt :: Maybe Text,
    livemode :: Bool,
    mandate :: Maybe Text,
    nextAction :: Maybe Value,
    onBehalfOf :: Maybe Stripe.AccountId,
    paymentMethod :: Maybe Stripe.PaymentMethodId,
    paymentMethodOptions :: Maybe PaymentMethodOptions,
    paymentMethodTypes :: [Text],
    singleUseMandate :: Maybe Text,
    status :: Stripe.PaymentIntentStatus,
    usage :: Maybe Text
  }
  deriving stock (Show)

data AutomaticPaymentMethods = AutomaticPaymentMethods
  { enabled :: Bool,
    allowRedirects :: Stripe.AutomaticPaymentMethodsAllowRedirects
  }
  deriving stock (Show)

newtype PaymentMethodOptions = PaymentMethodOptions
  { acssDebit :: Maybe ACSSDebit
  }
  deriving stock (Show)

data ACSSDebit = ACSSDebit
  { currency :: Currency,
    mandateOptions :: MandateOptions,
    verificationMethod :: Text
  }
  deriving stock (Show)

data MandateOptions = MandateOptions
  { intervalDescription :: Text,
    paymentSchedule :: Text,
    transactionType :: Text
  }
  deriving stock (Show)

--- PaymentIntent Object ---
data PaymentIntent = PaymentIntent
  { paymentIntentId :: Stripe.PaymentIntentId,
    orderShortId :: Maybe Text,
    amount :: HighPrecMoney,
    amountCapturable :: HighPrecMoney,
    amountReceived :: HighPrecMoney,
    application :: Maybe Text,
    applicationFeeAmount :: Maybe HighPrecMoney,
    automaticPaymentMethods :: Maybe AutomaticPaymentMethods,
    canceledAt :: Maybe UTCTime,
    cancellationReason :: Maybe Text,
    captureMethod :: Stripe.CaptureMethod,
    clientSecret :: Maybe Text,
    confirmationMethod :: Stripe.ConfirmationMethod,
    createdAt :: UTCTime,
    currency :: Currency,
    customer :: Maybe Stripe.CustomerId,
    description :: Maybe Text,
    invoice :: Maybe Text,
    lastPaymentError :: Maybe Value,
    latestCharge :: Maybe Text,
    livemode :: Bool,
    nextAction :: Maybe Value,
    onBehalfOf :: Maybe Stripe.AccountId,
    paymentMethod :: Maybe Stripe.PaymentMethodId,
    paymentMethodOptions :: Maybe Value,
    paymentMethodTypes :: [Text],
    processing :: Maybe Value,
    receiptEmail :: Maybe Text,
    review :: Maybe Text,
    setupFutureUsage :: Maybe Stripe.SetupFutureUsage,
    shipping :: Maybe Value,
    status :: Stripe.PaymentIntentStatus,
    transferGroup :: Maybe Text
  }
  deriving stock (Show)

--- Charge Object ---
data Charge = Charge
  { chargeId :: Stripe.ChargeId,
    paymentIntentId :: Maybe Stripe.PaymentIntentId,
    orderShortId :: Maybe Text,
    amount :: HighPrecMoney,
    amountCaptured :: HighPrecMoney,
    amountRefunded :: HighPrecMoney,
    application :: Maybe Text,
    applicationFeeAmount :: Maybe HighPrecMoney,
    balanceTransaction :: Maybe Text,
    calculatedStatementDescriptor :: Maybe Text,
    captured :: Bool,
    createdAt :: UTCTime,
    currency :: Currency,
    customer :: Maybe Stripe.CustomerId,
    description :: Maybe Text,
    disputed :: Bool,
    failureCode :: Maybe Text,
    failureMessage :: Maybe Text,
    fraudDetails :: Maybe Value,
    invoice :: Maybe Text,
    livemode :: Bool,
    outcome :: Maybe Value,
    paid :: Bool,
    paymentMethod :: Maybe Stripe.PaymentMethodId,
    receiptEmail :: Maybe Text,
    receiptUrl :: Maybe Text,
    refunded :: Bool,
    refunds :: Maybe Value, -- This would be a list of refund objects
    status :: Stripe.ChargeStatus
  }
  deriving stock (Show)

type Refund = GetRefundResp
