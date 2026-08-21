{-# LANGUAGE DerivingStrategies #-}
{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Payout.Types where

import Data.Aeson.Types
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto (derivePersistField)

data PayoutService = AAJuspay | Juspay | Stripe | StripeTest | HdfcCbx
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 'BulkFlow' partners have no single-order API: payouts are batched and submitted
-- together, then resolved by polling. See Kernel.External.Payout.Interface.HdfcCbx.
data PayoutServiceFlow = StripeFlow | JuspayFlow | BulkFlow
  deriving stock (Show, Generic, Eq)

castPayoutServiceFlow :: PayoutService -> PayoutServiceFlow
castPayoutServiceFlow Stripe = StripeFlow
castPayoutServiceFlow StripeTest = StripeFlow
castPayoutServiceFlow Juspay = JuspayFlow
castPayoutServiceFlow AAJuspay = JuspayFlow
castPayoutServiceFlow HdfcCbx = BulkFlow

$(mkBeamInstancesForEnumAndList ''PayoutService)
derivePersistField "PayoutService"
