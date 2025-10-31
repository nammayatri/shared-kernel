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

data AutomaticPayementMethods = AutomaticPayementMethods
  { enabled :: Bool,
    allow_redirects :: AutomaticPayementMethodsAllowRedirects
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AutomaticPayementMethodsAllowRedirects = AlwaysRedirect | NeverRedirect
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance ToHttpApiData AutomaticPayementMethodsAllowRedirects where
  toQueryParam :: AutomaticPayementMethodsAllowRedirects -> Text
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
