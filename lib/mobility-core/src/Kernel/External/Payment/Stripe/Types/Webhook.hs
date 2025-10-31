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
import qualified Data.Aeson as A
import qualified Data.Bimap as BM
import qualified Data.Map as M
import Data.OpenApi (ToSchema (declareNamedSchema), genericDeclareNamedSchema)
import Data.Time.Clock.POSIX (POSIXTime)
import Kernel.External.Payment.Stripe.Types.Common
import Kernel.Prelude
import Kernel.Types.HideSecrets
import Kernel.Types.Id
import Kernel.Types.Price
import qualified Kernel.Utils.JSON as J
import qualified Kernel.Utils.Schema as S

data EventType
  = -- Payment Intents
    PaymentIntentSucceeded
  | PaymentIntentPaymentFailed
  | PaymentIntentProcessing
  | PaymentIntentCanceled
  | PaymentIntentCreated
  | PaymentIntentRequiresAction
  | -- Setup Intents

    SetupIntentSucceeded
  | SetupIntentSetupFailed
  | SetupIntentCanceled
  | SetupIntentCreated
  | SetupIntentRequiresAction
  | -- Customers

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
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToSchema)

eventTypeBimap :: BM.Bimap EventType Text
eventTypeBimap =
  BM.fromList
    [ -- Payment Intents
      (PaymentIntentSucceeded, "payment_intent.succeeded"),
      (PaymentIntentPaymentFailed, "payment_intent.payment_failed"),
      (PaymentIntentProcessing, "payment_intent.processing"),
      (PaymentIntentCanceled, "payment_intent.canceled"),
      (PaymentIntentCreated, "payment_intent.created"),
      (PaymentIntentRequiresAction, "payment_intent.requires_action"),
      -- Setup Intents
      (SetupIntentSucceeded, "setup_intent.succeeded"),
      (SetupIntentSetupFailed, "setup_intent.setup_failed"),
      (SetupIntentCanceled, "setup_intent.canceled"),
      (SetupIntentCreated, "setup_intent.created"),
      (SetupIntentRequiresAction, "setup_intent.requires_action"),
      -- Customers
      -- (CustomerCreated, "customer.created"),
      -- (CustomerUpdated, "customer.updated"),
      -- (CustomerDeleted, "customer.deleted"),
      -- (CustomerSourceCreated, "customer.source.created"),
      -- (CustomerSourceUpdated, "customer.source.updated"),
      -- (CustomerSourceExpiring, "customer.source.expiring"),

      -- Subscriptions
      -- (CustomerSubscriptionCreated, "customer.subscription.created"),
      -- (CustomerSubscriptionUpdated, "customer.subscription.updated"),
      -- (CustomerSubscriptionDeleted, "customer.subscription.deleted"),
      -- (CustomerSubscriptionTrialWillEnd, "customer.subscription.trial_will_end"),
      -- (CustomerSubscriptionPendingUpdateApplied, "customer.subscription.pending_update_applied"),
      -- (CustomerSubscriptionPendingUpdateExpired, "customer.subscription.pending_update_expired"),

      -- Invoices
      -- (InvoiceCreated, "invoice.created"),
      -- (InvoiceFinalized, "invoice.finalized"),
      -- (InvoicePaymentSucceeded, "invoice.payment_succeeded"),
      -- (InvoicePaymentFailed, "invoice.payment_failed"),
      -- (InvoiceUpcoming, "invoice.upcoming"),
      -- (InvoiceMarkedUncollectible, "invoice.marked_uncollectible"),

      -- Charges
      (ChargeSucceeded, "charge.succeeded"),
      (ChargeFailed, "charge.failed"),
      (ChargeRefunded, "charge.refunded"),
      (ChargeDisputeCreated, "charge.dispute.created"),
      (ChargeDisputeClosed, "charge.dispute.closed"),
      -- Payment Methods
      -- (PaymentMethodAttached, "payment_method.attached"),
      -- (PaymentMethodAutomaticallyUpdated, "payment_method.automatically_updated"),
      -- (PaymentMethodDetached, "payment_method.detached"),

      -- Refunds
      (ChargeRefundUpdated, "charge.refund.updated")
      -- Account
      -- (AccountUpdated, "account.updated"),
      -- (AccountApplicationAuthorized, "account.application.authorized"),
      -- (AccountApplicationDeauthorized, "account.application.deauthorized"),

      -- Connect
      -- (PayoutCreated, "payout.created"),
      -- (PayoutPaid, "payout.paid"),
      -- (PayoutFailed, "payout.failed"),
      -- (PayoutCanceled, "payout.canceled"),

      -- Tax
      -- (TaxRateCreated, "tax_rate.created"),
      -- (TaxRateUpdated, "tax_rate.updated"),

      -- Billing
      -- (BillingPortalConfigurationCreated, "billing_portal.configuration.created"),
      -- (BillingPortalConfigurationUpdated, "billing_portal.configuration.updated"),

      -- Checkout
      -- (CheckoutSessionCompleted, "checkout.session.completed"),
      -- (CheckoutSessionAsyncPaymentSucceeded, "checkout.session.async_payment_succeeded"),
      -- (CheckoutSessionAsyncPaymentFailed, "checkout.session.async_payment_failed"),
      -- (CheckoutSessionExpired, "checkout.session.expired"),

      -- Price
      -- (PriceCreated, "price.created"),
      -- (PriceUpdated, "price.updated"),
      -- (PriceDeleted, "price.deleted"),

      -- Product
      -- (ProductCreated, "product.created"),
      -- (ProductUpdated, "product.updated"),
      -- (ProductDeleted, "product.deleted")
    ]

instance FromJSON EventType where
  parseJSON = withText "EventType" $ \txt ->
    pure $ fromMaybe (CustomEvent txt) $ BM.lookupR txt eventTypeBimap

instance ToJSON EventType where
  toJSON = String . eventTypeToText

eventTypeToText :: EventType -> Text
eventTypeToText eventType = case BM.lookup eventType eventTypeBimap of
  Just txt -> txt
  Nothing -> show eventType -- impossible: missing mapping

data WebhookReq = WebhookReq
  { id :: Id Event,
    _object :: Text,
    api_version :: Text, -- should be configurated in stripe dashboard
    created :: POSIXTime,
    _data :: WebhookReqData,
    livemode :: Bool,
    pending_webhooks :: Integer,
    request :: WebhookRequest,
    _type :: EventType
  }
  deriving stock (Show, Generic)

instance HideSecrets WebhookReq where
  hideSecrets WebhookReq {..} =
    WebhookReq
      { _data = hideSecrets @WebhookReqData _data,
        ..
      }

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

instance HideSecrets WebhookReqData where
  hideSecrets WebhookReqData {..} =
    WebhookReqData
      { _object = hideSecrets @StripeObject _object
      }

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
  = ObjectPaymentIntent PaymentIntent
  | ObjectSetupIntent SetupIntent
  | --   | ObjectCustomer Customer
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

getObjectType :: StripeObject -> Text
getObjectType = \case
  ObjectPaymentIntent obj -> obj._object
  ObjectSetupIntent obj -> obj._object
  ObjectCharge obj -> obj._object
  CustomObject objType _val -> objType

instance HideSecrets StripeObject where
  hideSecrets = \case
    ObjectSetupIntent a -> ObjectSetupIntent $ hideSecrets @SetupIntent a
    ObjectPaymentIntent a -> ObjectPaymentIntent $ hideSecrets @PaymentIntent a
    ObjectCharge a -> ObjectCharge $ hideSecrets @Charge a
    CustomObject objType _val -> CustomObject objType A.Null

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
    automatic_payment_methods :: Maybe AutomaticPaymentMethods,
    cancellation_reason :: Maybe Text,
    client_secret :: Maybe Text,
    created :: POSIXTime,
    customer :: Maybe CustomerId,
    description :: Maybe Text,
    flow_directions :: Maybe [Text],
    last_setup_error :: Maybe Value,
    latest_attempt :: Maybe Text,
    livemode :: Bool,
    mandate :: Maybe Text,
    metadata :: Maybe (M.Map Text Text),
    next_action :: Maybe Value,
    on_behalf_of :: Maybe AccountId,
    payment_method :: Maybe PaymentMethodId,
    payment_method_options :: Maybe PaymentMethodOptions,
    payment_method_types :: [Text],
    single_use_mandate :: Maybe Text,
    status :: PaymentIntentStatus,
    usage :: Maybe Text
  }
  deriving stock (Show, Generic)

instance HideSecrets SetupIntent where
  hideSecrets SetupIntent {..} =
    SetupIntent
      { client_secret = Nothing,
        customer = Nothing,
        last_setup_error = Nothing,
        metadata = Nothing,
        payment_method = Nothing,
        ..
      }

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
  { currency :: Currency,
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
    amount :: Int,
    amount_capturable :: Int,
    amount_received :: Int,
    application :: Maybe Text,
    application_fee_amount :: Maybe Int,
    automatic_payment_methods :: Maybe AutomaticPaymentMethods,
    canceled_at :: Maybe POSIXTime,
    cancellation_reason :: Maybe Text,
    capture_method :: CaptureMethod,
    client_secret :: Maybe Text,
    confirmation_method :: ConfirmationMethod,
    created :: POSIXTime,
    currency :: Currency,
    customer :: Maybe CustomerId,
    description :: Maybe Text,
    invoice :: Maybe Text,
    last_payment_error :: Maybe Value,
    latest_charge :: Maybe Text,
    livemode :: Bool,
    metadata :: Maybe (M.Map Text Text),
    next_action :: Maybe Value,
    on_behalf_of :: Maybe AccountId,
    payment_method :: Maybe PaymentMethodId,
    payment_method_options :: Maybe Value,
    payment_method_types :: [Text],
    processing :: Maybe Value,
    receipt_email :: Maybe Text,
    review :: Maybe Text,
    setup_future_usage :: Maybe SetupFutureUsage,
    shipping :: Maybe Value,
    status :: PaymentIntentStatus,
    transfer_group :: Maybe Text
  }
  deriving stock (Show, Generic)

instance HideSecrets PaymentIntent where
  hideSecrets PaymentIntent {..} =
    PaymentIntent
      { client_secret = Nothing,
        customer = Nothing,
        last_payment_error = Nothing,
        metadata = Nothing,
        payment_method = Nothing,
        receipt_email = Nothing,
        shipping = Nothing,
        ..
      }

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
    amount :: Int,
    amount_captured :: Int,
    amount_refunded :: Int,
    application :: Maybe Text,
    application_fee_amount :: Maybe Int,
    balance_transaction :: Maybe Text,
    calculated_statement_descriptor :: Maybe Text,
    captured :: Bool,
    created :: POSIXTime,
    currency :: Currency,
    customer :: Maybe CustomerId,
    description :: Maybe Text,
    disputed :: Bool,
    failure_code :: Maybe Text,
    failure_message :: Maybe Text,
    fraud_details :: Maybe Value,
    invoice :: Maybe Text,
    livemode :: Bool,
    metadata :: Maybe (M.Map Text Text),
    outcome :: Maybe Value,
    paid :: Bool,
    payment_intent :: Maybe Text,
    payment_method :: Maybe PaymentMethodId,
    receipt_email :: Maybe Text,
    receipt_url :: Maybe Text,
    refunded :: Bool,
    refunds :: Maybe Value, -- This would be a list of refund objects
    status :: ChargeStatus
  }
  deriving stock (Show, Generic)

instance HideSecrets Charge where
  hideSecrets Charge {..} =
    Charge
      { customer = Nothing,
        failure_message = Nothing,
        fraud_details = Nothing,
        metadata = Nothing,
        outcome = Nothing,
        payment_method = Nothing,
        receipt_email = Nothing,
        receipt_url = Nothing,
        ..
      }

instance FromJSON Charge where
  parseJSON = genericParseJSON J.stripPrefixUnderscoreIfAny

instance ToJSON Charge where
  toJSON = genericToJSON J.stripPrefixUnderscoreIfAny

instance ToSchema Charge where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny
