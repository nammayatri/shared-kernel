{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Stripe.Types.PaymentIntent where

import Data.Aeson
import Kernel.External.Payment.Stripe.Types.Common
import Kernel.Prelude
import Kernel.Types.Price

data CreatePaymentIntentReq = CreatePaymentIntentReq
  { amount :: Int,
    currency :: Currency,
    automatic_payment_methods :: AutomaticPayementMethods,
    confirm :: Bool,
    customer_id :: CustomerId,
    description :: Maybe Text,
    payment_method :: Text,
    receipt_email :: Maybe Text,
    setup_future_usage :: Maybe SetupFutureUsage,
    application_fee_amount :: Int,
    capture_method :: CaptureMethod,
    confirmation_method :: ConfirmationMethod,
    on_behalf_of :: AccountId,
    use_stripe_sdk :: Bool
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AutomaticPayementMethods = AutomaticPayementMethods
  { enabled :: Bool,
    allow_redirects :: AutomaticPayementMethodsAllowRedirects
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AutomaticPayementMethodsAllowRedirects = AlwaysRedirect | NeverRedirect
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

automaticPayementMethodsAllowRedirectsJsonOptions :: Options
automaticPayementMethodsAllowRedirectsJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "AlwaysRedirect" -> "redirect"
        "NeverRedirect" -> "never"
        x -> x
    }

instance FromJSON AutomaticPayementMethodsAllowRedirects where
  parseJSON = genericParseJSON automaticPayementMethodsAllowRedirectsJsonOptions

instance ToJSON AutomaticPayementMethodsAllowRedirects where
  toJSON = genericToJSON automaticPayementMethodsAllowRedirectsJsonOptions

data SetupFutureUsage = FutureUsageOffSession | FutureUsageOnSession
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

setupFutureUsageJsonOptions :: Options
setupFutureUsageJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "FutureUsageOffSession" -> "off_session"
        "FutureUsageOnSession" -> "on_session"
        x -> x
    }

instance FromJSON SetupFutureUsage where
  parseJSON = genericParseJSON setupFutureUsageJsonOptions

instance ToJSON SetupFutureUsage where
  toJSON = genericToJSON setupFutureUsageJsonOptions

data CaptureMethod = AutomaticCaptureMethod | AutomaticAsyncCaptureMethod | ManualCaptureMethod
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

captureMethodJsonOptions :: Options
captureMethodJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "AutomaticCaptureMethod" -> "automatic"
        "AutomaticAsyncCaptureMethod" -> "automatic_async"
        "ManualCaptureMethod" -> "manual"
        x -> x
    }

instance FromJSON CaptureMethod where
  parseJSON = genericParseJSON captureMethodJsonOptions

instance ToJSON CaptureMethod where
  toJSON = genericToJSON captureMethodJsonOptions

data ConfirmationMethod = AutomaticConfirmationMethod | ManualConfirmationMethod
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

confirmationMethodJsonOptions :: Options
confirmationMethodJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "AutomaticConfirmationMethod" -> "automatic"
        "ManualConfirmationMethod" -> "manual"
        x -> x
    }

instance FromJSON ConfirmationMethod where
  parseJSON = genericParseJSON confirmationMethodJsonOptions

instance ToJSON ConfirmationMethod where
  toJSON = genericToJSON confirmationMethodJsonOptions

data PaymentIntentObject = PaymentIntentObject
  { id :: PaymentIntentId,
    amount :: Int,
    currency :: Currency,
    client_secret :: Text,
    latest_charge :: Maybe Text,
    status :: PaymentIntentStatus,
    automatic_payment_methods :: AutomaticPayementMethods,
    confirm :: Bool,
    customer_id :: CustomerId,
    description :: Maybe Text,
    payment_method :: Text,
    receipt_email :: Maybe Text,
    setup_future_usage :: Maybe SetupFutureUsage,
    application_fee_amount :: Int,
    capture_method :: CaptureMethod,
    confirmation_method :: ConfirmationMethod,
    on_behalf_of :: AccountId,
    use_stripe_sdk :: Bool
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PaymentIntentStatus = Cancelled | Processing | RequiresAction | RequiresCapture | RequiresConfirmation | RequiresPaymentMethod | Succeeded
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

paymentIntentStatusJsonOptions :: Options
paymentIntentStatusJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "Cancelled" -> "cancelled"
        "Processing" -> "processing"
        "RequiresAction" -> "requires_action"
        "RequiresCapture" -> "requires_capture"
        "RequiresConfirmation" -> "requires_confirmation"
        "RequiresPaymentMethod" -> "requires_payment_method"
        "Succeeded" -> "succeeded"
        x -> x
    }

instance FromJSON PaymentIntentStatus where
  parseJSON = genericParseJSON paymentIntentStatusJsonOptions

instance ToJSON PaymentIntentStatus where
  toJSON = genericToJSON paymentIntentStatusJsonOptions
