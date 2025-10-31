{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

module Kernel.External.Payment.Stripe.Types.Common where

import Data.Aeson
import Kernel.Prelude
import Web.HttpApiData (ToHttpApiData (..))

type AccountId = Text

type CustomerId = Text

type PaymentIntentId = Text

type SetupIntentId = Text

type PaymentMethodId = Text

data Event

data AutomaticPaymentMethods = AutomaticPaymentMethods
  { enabled :: Bool,
    allow_redirects :: AutomaticPaymentMethodsAllowRedirects
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AutomaticPaymentMethodsAllowRedirects = AlwaysRedirect | NeverRedirect
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance ToHttpApiData AutomaticPaymentMethodsAllowRedirects where
  toQueryParam :: AutomaticPaymentMethodsAllowRedirects -> Text
  toQueryParam AlwaysRedirect = "redirect"
  toQueryParam NeverRedirect = "never"

automaticPayementMethodsAllowRedirectsJsonOptions :: Options
automaticPayementMethodsAllowRedirectsJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "AlwaysRedirect" -> "redirect"
        "NeverRedirect" -> "never"
        x -> x
    }

instance FromJSON AutomaticPaymentMethodsAllowRedirects where
  parseJSON = genericParseJSON automaticPayementMethodsAllowRedirectsJsonOptions

instance ToJSON AutomaticPaymentMethodsAllowRedirects where
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

instance ToHttpApiData SetupFutureUsage where
  toQueryParam :: SetupFutureUsage -> Text
  toQueryParam FutureUsageOffSession = "off_session"
  toQueryParam FutureUsageOnSession = "on_session"

instance FromJSON SetupFutureUsage where
  parseJSON = genericParseJSON setupFutureUsageJsonOptions

instance ToJSON SetupFutureUsage where
  toJSON = genericToJSON setupFutureUsageJsonOptions

data PaymentIntentStatus = Cancelled | Processing | RequiresAction | RequiresCapture | RequiresConfirmation | RequiresPaymentMethod | Succeeded
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

paymentIntentStatusJsonOptions :: Options
paymentIntentStatusJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "Cancelled" -> "canceled"
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

instance ToHttpApiData CaptureMethod where
  toQueryParam :: CaptureMethod -> Text
  toQueryParam AutomaticCaptureMethod = "automatic"
  toQueryParam AutomaticAsyncCaptureMethod = "automatic_async"
  toQueryParam ManualCaptureMethod = "manual"

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

instance ToHttpApiData ConfirmationMethod where
  toQueryParam :: ConfirmationMethod -> Text
  toQueryParam AutomaticConfirmationMethod = "automatic"
  toQueryParam ManualConfirmationMethod = "manual"

instance FromJSON ConfirmationMethod where
  parseJSON = genericParseJSON confirmationMethodJsonOptions

instance ToJSON ConfirmationMethod where
  toJSON = genericToJSON confirmationMethodJsonOptions

data ChargeStatus = CHARGE_SUCCEEDED | CHARGE_PENDING | CHARGE_FAILED | CHARGE_REFUNDED | CHARGE_DISPUTED | CHARGE_UNCAPTURED | CHARGE_CANCELED
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON ChargeStatus where
  parseJSON = genericParseJSON chargeStatusJsonOptions

instance ToJSON ChargeStatus where
  toJSON = genericToJSON chargeStatusJsonOptions

chargeStatusJsonOptions :: Options
chargeStatusJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "CHARGE_SUCCEEDED" -> "succeeded"
        "CHARGE_PENDING" -> "pending"
        "CHARGE_FAILED" -> "failed"
        "CHARGE_REFUNDED" -> "refunded"
        "CHARGE_DISPUTED" -> "disputed"
        "CHARGE_UNCAPTURED" -> "uncaptured"
        "CHARGE_CANCELED" -> "canceled"
        x -> x
    }
