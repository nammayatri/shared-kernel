{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Juspay.Types.UpdateOrder where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.Common
import Web.FormUrlEncoded

data OrderUpdateReq = OrderUpdateReq
  { amount :: HighPrecMoney
  }
  deriving (Show, Generic)
  deriving anyclass (ToSchema, FromJSON, ToJSON, ToForm)

data OrderUpdateResp = OrderUpdateResp
  { order_id :: Maybe Text,
    amount :: Maybe HighPrecMoney
  }
  deriving (Show, Generic)
  deriving anyclass (ToSchema, FromJSON, ToJSON)
