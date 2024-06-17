{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Stripe.Types.Customer where

import Data.Aeson
import Kernel.External.Payment.Stripe.Types.Common
import Kernel.Prelude

data CustomerReq = CustomerReq
  { email :: Text,
    name :: Text,
    payment_method :: Maybe Text,
    source :: Maybe Text,
    phone :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data UpdateCustomerReq = UpdateCustomerReq
  { email :: Maybe Text,
    name :: Maybe Text,
    source :: Maybe Text,
    phone :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CustomerObject = CustomerObject
  { id :: CustomerId,
    email :: Maybe Text,
    name :: Maybe Text,
    default_source :: Maybe Text,
    phone :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype EphemeralKeysReq = EphemeralKeysReq
  { customer :: CustomerId
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype EphemeralKeysResp = EphemeralKeysResp
  { secret :: Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
