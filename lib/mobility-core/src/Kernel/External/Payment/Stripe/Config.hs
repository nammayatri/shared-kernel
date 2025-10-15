{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Stripe.Config
  ( module Kernel.External.Payment.Stripe.Config,
    module Reexport,
  )
where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.External.Payment.Stripe.Types.Accounts as Reexport (BusinessProfile (..))
import Kernel.Prelude

data ChargeDestination = Platform | ConnectedAccount
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON)

data StripeCfg = StripeCfg
  { apiKey :: EncryptedField 'AsEncrypted Text,
    returnUrl :: BaseUrl,
    refreshUrl :: BaseUrl,
    url :: BaseUrl,
    businessProfile :: Maybe BusinessProfile,
    chargeDestination :: ChargeDestination
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
