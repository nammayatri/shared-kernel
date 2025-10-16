{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Stripe.Types.Webhook where

import Data.Aeson
import qualified Data.Map as M
import Data.OpenApi (ToSchema (declareNamedSchema), genericDeclareNamedSchema)
import Kernel.Prelude
import Kernel.Types.Id
import qualified Kernel.Utils.JSON as J
import qualified Kernel.Utils.Schema as S

-- TODO remove or make optional all not mandatory fields

data Event

data EventType
  = -- Payment Intents
    PaymentIntentSucceeded
  | PaymentIntentPaymentFailed
  | PaymentIntentProcessing
  | PaymentIntentCanceled
  | PaymentIntentCreated
  | PaymentIntentRequiresAction
  | -- Setup Intents

    -- | SetupIntentSucceeded
    -- | SetupIntentSetupFailed
    -- | SetupIntentCanceled
    -- | SetupIntentCreated
    -- | SetupIntentRequiresAction
    -- Customers
    -- | CustomerCreated
    -- | CustomerUpdated
    -- | CustomerDeleted
    -- | CustomerSourceCreated
    -- | CustomerSourceUpdated
    -- | CustomerSourceExpiring
    -- Subscriptions
    -- | CustomerSubscriptionCreated
    -- | CustomerSubscriptionUpdated
    -- | CustomerSubscriptionDeleted
    -- | CustomerSubscriptionTrialWillEnd
    -- | CustomerSubscriptionPendingUpdateApplied
    -- | CustomerSubscriptionPendingUpdateExpired
    -- Invoices
    -- |  InvoiceCreated
    -- | InvoiceFinalized
    -- | InvoicePaymentSucceeded
    -- | InvoicePaymentFailed
    -- | InvoiceUpcoming
    -- | InvoiceMarkedUncollectible
    -- Charges
    ChargeSucceeded
  | ChargeFailed
  | ChargeRefunded
  | ChargeDisputeCreated
  | ChargeDisputeClosed
  | -- Payment Methods

    -- | PaymentMethodAttached
    -- | PaymentMethodAutomaticallyUpdated
    -- | PaymentMethodDetached
    -- Refunds
    ChargeRefundUpdated
  | -- Account

    -- | AccountUpdated
    -- | AccountApplicationAuthorized
    -- | AccountApplicationDeauthorized
    -- Connect
    -- | PayoutCreated
    -- | PayoutPaid
    -- | PayoutFailed
    -- | PayoutCanceled
    -- Tax
    -- | TaxRateCreated
    -- | TaxRateUpdated
    -- Billing
    -- | BillingPortalConfigurationCreated
    -- | BillingPortalConfigurationUpdated
    -- Checkout
    -- | CheckoutSessionCompleted
    -- | CheckoutSessionAsyncPaymentSucceeded
    -- | CheckoutSessionAsyncPaymentFailed
    -- | CheckoutSessionExpired
    -- Price
    -- | PriceCreated
    -- | PriceUpdated
    -- | PriceDeleted
    -- Product
    -- | ProductCreated
    -- | ProductUpdated
    -- | ProductDeleted
    -- Custom (for unknown types)
    CustomEvent Text
  deriving (Show, Eq, Generic)

eventTypeFromText :: Text -> EventType
eventTypeFromText = \case
  -- Payment Intents
  "payment_intent.succeeded" -> PaymentIntentSucceeded
  "payment_intent.payment_failed" -> PaymentIntentPaymentFailed
  "payment_intent.processing" -> PaymentIntentProcessing
  "payment_intent.canceled" -> PaymentIntentCanceled
  "payment_intent.created" -> PaymentIntentCreated
  "payment_intent.requires_action" -> PaymentIntentRequiresAction
  -- Setup Intents
  -- "setup_intent.succeeded" -> SetupIntentSucceeded
  -- "setup_intent.setup_failed" -> SetupIntentSetupFailed
  -- "setup_intent.canceled" -> SetupIntentCanceled
  -- "setup_intent.created" -> SetupIntentCreated
  -- "setup_intent.requires_action" -> SetupIntentRequiresAction

  -- Customers
  -- "customer.created" -> CustomerCreated
  -- "customer.updated" -> CustomerUpdated
  -- "customer.deleted" -> CustomerDeleted
  -- "customer.source.created" -> CustomerSourceCreated
  -- "customer.source.updated" -> CustomerSourceUpdated
  -- "customer.source.expiring" -> CustomerSourceExpiring

  -- Subscriptions
  -- "customer.subscription.created" -> CustomerSubscriptionCreated
  -- "customer.subscription.updated" -> CustomerSubscriptionUpdated
  -- "customer.subscription.deleted" -> CustomerSubscriptionDeleted
  -- "customer.subscription.trial_will_end" -> CustomerSubscriptionTrialWillEnd
  -- "customer.subscription.pending_update_applied" -> CustomerSubscriptionPendingUpdateApplied
  -- "customer.subscription.pending_update_expired" -> CustomerSubscriptionPendingUpdateExpired

  -- Invoices
  -- "invoice.created" -> InvoiceCreated
  -- "invoice.finalized" -> InvoiceFinalized
  -- "invoice.payment_succeeded" -> InvoicePaymentSucceeded
  -- "invoice.payment_failed" -> InvoicePaymentFailed
  -- "invoice.upcoming" -> InvoiceUpcoming
  -- "invoice.marked_uncollectible" -> InvoiceMarkedUncollectible

  -- Charges
  "charge.succeeded" -> ChargeSucceeded
  "charge.failed" -> ChargeFailed
  "charge.refunded" -> ChargeRefunded
  "charge.dispute.created" -> ChargeDisputeCreated
  "charge.dispute.closed" -> ChargeDisputeClosed
  -- Payment Methods
  -- "payment_method.attached" -> PaymentMethodAttached
  -- "payment_method.automatically_updated" -> PaymentMethodAutomaticallyUpdated
  -- "payment_method.detached" -> PaymentMethodDetached

  -- Refunds
  "charge.refund.updated" -> ChargeRefundUpdated
  -- Account
  -- "account.updated" -> AccountUpdated
  -- "account.application.authorized" -> AccountApplicationAuthorized
  -- "account.application.deauthorized" -> AccountApplicationDeauthorized

  -- Connect
  -- "payout.created" -> PayoutCreated
  -- "payout.paid" -> PayoutPaid
  -- "payout.failed" -> PayoutFailed
  -- "payout.canceled" -> PayoutCanceled

  -- Tax
  -- "tax_rate.created" -> TaxRateCreated
  -- "tax_rate.updated" -> TaxRateUpdated

  -- Billing
  -- "billing_portal.configuration.created" -> BillingPortalConfigurationCreated
  -- "billing_portal.configuration.updated" -> BillingPortalConfigurationUpdated

  -- Checkout
  -- "checkout.session.completed" -> CheckoutSessionCompleted
  -- "checkout.session.async_payment_succeeded" -> CheckoutSessionAsyncPaymentSucceeded
  -- "checkout.session.async_payment_failed" -> CheckoutSessionAsyncPaymentFailed
  -- "checkout.session.expired" -> CheckoutSessionExpired

  -- Price
  -- "price.created" -> PriceCreated
  -- "price.updated" -> PriceUpdated
  -- "price.deleted" -> PriceDeleted

  -- Product
  -- "product.created" -> ProductCreated
  -- "product.updated" -> ProductUpdated
  -- "product.deleted" -> ProductDeleted

  -- Unknown events
  unknown -> CustomEvent unknown

data WebhookReq = WebhookReq
  { id :: Id Event,
    _object :: Text,
    api_version :: Text, -- should be configurated in stripe dashboard
    created :: Integer,
    _data :: WebhookReqData,
    livemode :: Bool,
    pending_webhooks :: Integer,
    request :: WebhookRequest,
    _type :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON WebhookReq where
  parseJSON = genericParseJSON J.stripPrefixUnderscoreIfAny

instance ToJSON WebhookReq where
  toJSON = genericToJSON J.stripPrefixUnderscoreIfAny

instance ToSchema WebhookReq where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

newtype WebhookReqData = WebhookReqData
  { _object :: StripeObject
  }
  deriving stock (Show, Generic)

instance FromJSON WebhookReqData where
  parseJSON = genericParseJSON J.stripPrefixUnderscoreIfAny

instance ToJSON WebhookReqData where
  toJSON = genericToJSON J.stripPrefixUnderscoreIfAny

instance ToSchema WebhookReqData where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

data WebhookRequest = WebhookRequest
  { id :: Maybe Text,
    idempotency_key :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data StripeObject
  = ObjectSetupIntent SetupIntent
  | ObjectPaymentIntent PaymentIntent
  | --   | ObjectSetupIntent SetupIntent
    --   | ObjectCustomer Customer
    --   | ObjectSubscription Subscription
    --   | ObjectInvoice Invoice
    ObjectCharge Charge
  | --   | ObjectPaymentMethod PaymentMethod
    --   | ObjectRefund Refund
    --   | ObjectAccount Account
    --   | ObjectPayout Payout
    --   | ObjectTaxRate TaxRate
    --   | ObjectBillingPortalConfiguration BillingPortalConfiguration
    --   | ObjectCheckoutSession CheckoutSession
    --   | ObjectPrice Price
    --   | ObjectProduct Product
    --   | ObjectBalanceTransaction BalanceTransaction
    --   | ObjectCoupon Coupon
    --   | ObjectDiscount Discount
    --   | ObjectFile File
    --   | ObjectInvoiceItem InvoiceItem
    --   | ObjectPlan Plan
    --   | ObjectSource Source
    --   | ObjectTransfer Transfer
    --   | ObjectApplicationFee ApplicationFee
    --   | ObjectCapability Capability
    --   | ObjectCard Card
    --   | ObjectPerson Person
    --   | ObjectToken Token
    --   | ObjectWebhookEndpoint WebhookEndpoint
    CustomObject Text Value
  deriving (Show, Generic)
  deriving anyclass (ToSchema)

instance ToJSON StripeObject where
  toJSON = \case
    ObjectSetupIntent a -> toJSON @SetupIntent a
    ObjectPaymentIntent a -> toJSON @PaymentIntent a
    ObjectCharge a -> toJSON @Charge a
    CustomObject _objType val -> val

instance FromJSON StripeObject where
  parseJSON val = flip (withObject "StripeObject") val $ \obj -> do
    objectType :: Text <- obj .: "object"
    case objectType of
      "setup_intent" -> ObjectSetupIntent <$> parseJSON @SetupIntent val
      "payment_intent" -> ObjectPaymentIntent <$> parseJSON @PaymentIntent val
      "charge" -> ObjectCharge <$> parseJSON @Charge val
      unknown -> pure $ CustomObject unknown val

--- SetupIntent Object ---
data SetupIntent = SetupIntent
  { id :: Text,
    _object :: Text,
    application :: Maybe Text,
    automatic_payment_methods :: Maybe Value,
    cancellation_reason :: Maybe Text,
    client_secret :: Text,
    created :: Integer,
    customer :: Maybe Text,
    description :: Maybe Text,
    flow_directions :: Maybe [Text],
    last_setup_error :: Maybe Value,
    latest_attempt :: Maybe Text,
    livemode :: Bool,
    mandate :: Maybe Text,
    metadata :: M.Map Text Text,
    next_action :: Maybe Value,
    on_behalf_of :: Maybe Text,
    payment_method :: Maybe Text,
    payment_method_options :: PaymentMethodOptions,
    payment_method_types :: [Text],
    single_use_mandate :: Maybe Text,
    status :: Text,
    usage :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON SetupIntent where
  parseJSON = genericParseJSON J.stripPrefixUnderscoreIfAny

instance ToJSON SetupIntent where
  toJSON = genericToJSON J.stripPrefixUnderscoreIfAny

instance ToSchema SetupIntent where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

newtype PaymentMethodOptions = PaymentMethodOptions
  { acss_debit :: Maybe ACSSDebit
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data ACSSDebit = ACSSDebit
  { currency :: Text,
    mandate_options :: MandateOptions,
    verification_method :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data MandateOptions = MandateOptions
  { interval_description :: Text,
    payment_schedule :: Text,
    transaction_type :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

--- PaymentIntent Object ---
data PaymentIntent = PaymentIntent
  { id :: Text,
    _object :: Text,
    amount :: Integer,
    amount_capturable :: Integer,
    amount_received :: Integer,
    application :: Maybe Text,
    application_fee_amount :: Maybe Integer,
    automatic_payment_methods :: Maybe Value,
    canceled_at :: Maybe Integer,
    cancellation_reason :: Maybe Text,
    capture_method :: Text,
    client_secret :: Text,
    confirmation_method :: Text,
    created :: Integer,
    currency :: Text,
    customer :: Maybe Text,
    description :: Maybe Text,
    invoice :: Maybe Text,
    last_payment_error :: Maybe Value,
    latest_charge :: Maybe Text,
    livemode :: Bool,
    metadata :: M.Map Text Text,
    next_action :: Maybe Value,
    on_behalf_of :: Maybe Text,
    payment_method :: Maybe Text,
    payment_method_options :: Value,
    payment_method_types :: [Text],
    processing :: Maybe Value,
    receipt_email :: Maybe Text,
    review :: Maybe Text,
    setup_future_usage :: Maybe Text,
    shipping :: Maybe Value,
    status :: Text,
    transfer_group :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON PaymentIntent where
  parseJSON = genericParseJSON J.stripPrefixUnderscoreIfAny

instance ToJSON PaymentIntent where
  toJSON = genericToJSON J.stripPrefixUnderscoreIfAny

instance ToSchema PaymentIntent where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

--- Charge Object ---
data Charge = Charge
  { id :: Text,
    _object :: Text,
    amount :: Integer,
    amount_captured :: Integer,
    amount_refunded :: Integer,
    application :: Maybe Text,
    application_fee_amount :: Maybe Integer,
    balance_transaction :: Maybe Text,
    calculated_statement_descriptor :: Maybe Text,
    captured :: Bool,
    created :: Integer,
    currency :: Text,
    customer :: Maybe Text,
    description :: Maybe Text,
    disputed :: Bool,
    failure_code :: Maybe Text,
    failure_message :: Maybe Text,
    fraud_details :: Maybe Value,
    invoice :: Maybe Text,
    livemode :: Bool,
    metadata :: M.Map Text Text,
    outcome :: Maybe Value,
    paid :: Bool,
    payment_intent :: Maybe Text,
    payment_method :: Maybe Text,
    receipt_email :: Maybe Text,
    receipt_url :: Maybe Text,
    refunded :: Bool,
    refunds :: Value, -- This would be a list of refund objects
    status :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON Charge where
  parseJSON = genericParseJSON J.stripPrefixUnderscoreIfAny

instance ToJSON Charge where
  toJSON = genericToJSON J.stripPrefixUnderscoreIfAny

instance ToSchema Charge where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny
