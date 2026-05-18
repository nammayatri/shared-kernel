{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payout.Interface.Events.Types where

import qualified Kernel.External.Payment.Stripe.Types.Common as Stripe
import qualified Kernel.External.Payout.Stripe.Types.Payout as PayoutStripe
import qualified Kernel.External.Payout.Stripe.Types.Transfer as TransferStripe
import qualified Kernel.External.Payout.Stripe.Types.Webhook as PayoutWh
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data PayoutServiceEventResp = PayoutServiceEventResp
  { id :: Id Stripe.Event,
    apiVersion :: Text,
    createdAt :: UTCTime,
    eventData :: EventObject,
    livemode :: Bool,
    pendingWebhooks :: Integer,
    eventType :: PayoutWh.PayoutStripeWebhookEventType
  }
  deriving stock (Show)

data EventObject
  = PayoutCanceledEvent Payout
  | PayoutCreatedEvent Payout
  | PayoutFailedEvent Payout
  | PayoutPaidEvent Payout
  | PayoutReconciliationCompletedEvent Payout
  | PayoutUpdatedEvent Payout
  | TransferCreatedEvent Transfer
  | TransferReversedEvent Transfer
  | TransferUpdatedEvent Transfer
  | CustomEvent Text
  deriving stock (Show)

data Payout = Payout
  { payoutId :: PayoutStripe.PayoutId,
    orderId :: Maybe Text,
    customerId :: Maybe Text,
    orderType :: Maybe Text,
    amount :: HighPrecMoney,
    currency :: Text,
    status :: PayoutStripe.ExternalPayoutStatus,
    payoutType :: PayoutStripe.PayoutType,
    method :: PayoutStripe.PayoutMethod,
    description :: Maybe Text,
    destination :: Maybe Text,
    createdAt :: UTCTime,
    arrivalDate :: Maybe UTCTime,
    statementDescriptor :: Maybe Text,
    failureCode :: Maybe Text,
    failureMessage :: Maybe Text
  }
  deriving stock (Show)

data Transfer = Transfer
  { transferId :: TransferStripe.TransferId,
    amount :: HighPrecMoney,
    currency :: Text,
    destination :: Stripe.AccountId,
    createdAt :: UTCTime
  }
  deriving stock (Show)
