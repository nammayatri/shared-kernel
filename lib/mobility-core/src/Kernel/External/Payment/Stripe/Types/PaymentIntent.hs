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

data PaymentIntentReq = PaymentIntentReq
  { amount :: Int,
    currency :: Currency,
    automatic_payment_methods :: AutomaticPayementMethods,
    confirm :: Bool,
    customer :: CustomerId,
    description :: Maybe Text,
    payment_method :: PaymentMethodId,
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
    amount :: Maybe Int,
    currency :: Maybe Currency,
    client_secret :: Text,
    latest_charge :: Maybe Text,
    status :: PaymentIntentStatus,
    automatic_payment_methods :: Maybe AutomaticPayementMethods,
    confirm :: Maybe Bool,
    customer_id :: Maybe CustomerId,
    description :: Maybe Text,
    payment_method :: Maybe Text,
    receipt_email :: Maybe Text,
    setup_future_usage :: Maybe SetupFutureUsage,
    application_fee_amount :: Maybe Int,
    capture_method :: Maybe CaptureMethod,
    confirmation_method :: Maybe ConfirmationMethod,
    on_behalf_of :: Maybe AccountId,
    use_stripe_sdk :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype ConfirmPaymentIntentReq = ConfirmPaymentIntentReq
  { payment_method :: PaymentMethodId
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CapturePaymentIntentReq = CapturePaymentIntentReq
  { amount_to_capture :: Int,
    application_fee_amount :: Int
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data IncrementAuthorizationReq = IncrementAuthorizationReq
  { amount :: Int,
    application_fee_amount :: Int
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
