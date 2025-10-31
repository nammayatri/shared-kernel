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

module Kernel.External.Payment.Stripe.Types.PaymentIntent where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Kernel.External.Payment.Stripe.Types.Common
import Kernel.Prelude
import Kernel.Types.Price
import Web.FormUrlEncoded
import Web.HttpApiData (ToHttpApiData (..))

data PaymentIntentReq = PaymentIntentReq
  { amount :: Int,
    currency :: Currency,
    -- automatic_payment_methods :: AutomaticPaymentMethods,
    confirm :: Bool,
    customer :: CustomerId,
    description :: Maybe Text,
    payment_method :: PaymentMethodId,
    receipt_email :: Maybe Text,
    setup_future_usage :: Maybe SetupFutureUsage,
    application_fee_amount :: Int,
    capture_method :: CaptureMethod,
    confirmation_method :: ConfirmationMethod,
    on_behalf_of :: Maybe AccountId,
    use_stripe_sdk :: Bool,
    return_url :: Text,
    transfer_data :: TransferData
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance ToForm PaymentIntentReq where
  toForm PaymentIntentReq {..} =
    Form $
      HM.fromListWith
        (++)
        [ ("amount", [toQueryParam amount]),
          ("currency", [show currency]),
          -- ("automatic_payment_methods[enabled]", [toQueryParam (automatic_payment_methods.enabled)]),
          -- ("automatic_payment_methods[allow_redirects]", [toQueryParam (automatic_payment_methods.allow_redirects)]),
          ("confirm", [toQueryParam confirm]),
          ("customer", [toQueryParam customer]),
          ("payment_method", [toQueryParam payment_method]),
          ("application_fee_amount", [toQueryParam application_fee_amount]),
          ("capture_method", [toQueryParam capture_method]),
          ("confirmation_method", [toQueryParam confirmation_method]),
          ("use_stripe_sdk", [toQueryParam use_stripe_sdk]),
          ("return_url", [toQueryParam return_url]),
          ("transfer_data[destination]", [toQueryParam transfer_data.destination])
        ]
        <> maybeToForm "description" description
        <> maybeToForm "receipt_email" receipt_email
        <> maybeToForm "setup_future_usage" setup_future_usage
        <> maybeToForm "on_behalf_of" on_behalf_of
    where
      maybeToForm :: ToHttpApiData a => Text -> Maybe a -> HM.HashMap Text [Text]
      maybeToForm key = maybe HM.empty (\value -> HM.singleton key [toQueryParam value])

newtype TransferData = TransferData
  { destination :: AccountId
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PaymentIntentObject = PaymentIntentObject
  { id :: PaymentIntentId,
    amount :: Maybe Int,
    client_secret :: Text,
    latest_charge :: Maybe Text,
    status :: PaymentIntentStatus,
    -- automatic_payment_methods :: Maybe AutomaticPaymentMethods,
    -- confirm :: Maybe Bool,
    customer_id :: Maybe CustomerId,
    description :: Maybe Text,
    payment_method :: Maybe Text,
    receipt_email :: Maybe Text,
    -- setup_future_usage :: Maybe SetupFutureUsage,
    application_fee_amount :: Maybe Int,
    -- capture_method :: Maybe CaptureMethod,
    -- confirmation_method :: Maybe ConfirmationMethod,
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

instance ToForm ConfirmPaymentIntentReq where
  toForm ConfirmPaymentIntentReq {..} = Form $ HM.singleton "payment_method" [toQueryParam payment_method]

data CapturePaymentIntentReq = CapturePaymentIntentReq
  { amount_to_capture :: Int,
    application_fee_amount :: Int
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance ToForm CapturePaymentIntentReq where
  toForm CapturePaymentIntentReq {..} =
    Form $
      HM.fromList
        [ ("amount_to_capture", [toQueryParam amount_to_capture]),
          ("application_fee_amount", [toQueryParam application_fee_amount])
        ]

data IncrementAuthorizationReq = IncrementAuthorizationReq
  { amount :: Int,
    application_fee_amount :: Int
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance ToForm IncrementAuthorizationReq where
  toForm IncrementAuthorizationReq {..} =
    Form $
      HM.fromList
        [ ("amount", [toQueryParam amount]),
          ("application_fee_amount", [toQueryParam application_fee_amount])
        ]
