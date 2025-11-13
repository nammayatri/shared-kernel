{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Stripe.Types.SetupIntent where

import qualified Data.HashMap.Strict as HM
import Kernel.External.Payment.Stripe.Types.Common
import Kernel.Prelude
import Web.FormUrlEncoded
import Web.HttpApiData (ToHttpApiData (..))

data SetupIntentReq = SetupIntentReq
  { automatic_payment_methods :: AutomaticPaymentMethods,
    confirm :: Bool,
    customer :: CustomerId,
    description :: Maybe Text,
    payment_method :: Maybe Text,
    usage :: Maybe SetupFutureUsage,
    use_stripe_sdk :: Bool
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance ToForm SetupIntentReq where
  toForm SetupIntentReq {..} =
    Form $
      HM.fromList
        [ ("automatic_payment_methods[enabled]", [toQueryParam (automatic_payment_methods.enabled)]),
          ("automatic_payment_methods[allow_redirects]", [toQueryParam (automatic_payment_methods.allow_redirects)]),
          ("confirm", [toQueryParam confirm]),
          ("customer", [toQueryParam customer]),
          ("use_stripe_sdk", [toQueryParam use_stripe_sdk])
        ]
        <> maybeToForm "description" description
        <> maybeToForm "payment_method" payment_method
        <> maybeToForm "usage" usage
    where
      maybeToForm :: ToHttpApiData a => Text -> Maybe a -> HM.HashMap Text [Text]
      maybeToForm key = maybe HM.empty (\value -> HM.singleton key [toQueryParam value])

data SetupIntentObject = SetupIntentObject
  { id :: SetupIntentId,
    client_secret :: Text,
    latest_charge :: Maybe Text,
    status :: PaymentIntentStatus,
    confirm :: Maybe Bool,
    customer :: Maybe CustomerId,
    description :: Maybe Text,
    payment_method :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
