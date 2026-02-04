{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Juspay.Types.Wallet where

import qualified Data.HashMap.Strict as HM
import Kernel.Prelude
import Web.FormUrlEncoded
import Web.HttpApiData (ToHttpApiData (..))

data CreateWalletReq = CreateWalletReq
  { command :: Text,
    device_id :: Text,
    gateway :: Text,
    payment_method :: Text,
    gateway_reference_id :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance ToForm CreateWalletReq where
  toForm CreateWalletReq {..} =
    Form $
      HM.fromList
        [ ("command", [toQueryParam command]),
          ("device_id", [toQueryParam device_id]),
          ("gateway", [toQueryParam gateway]),
          ("payment_method", [toQueryParam payment_method]),
          ("gateway_reference_id", [toQueryParam gateway_reference_id])
        ]

data CreateWalletResp = CreateWalletResp
  { token :: Text,
    linked :: Bool,
    id :: Text,
    current_balance :: Maybe Text,
    wallet :: Text,
    auth_params :: Maybe AuthParams,
    last_refreshed :: Maybe Text,
    sub_details :: [SubDetail],
    object :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AuthParams = AuthParams
  { is_resend_allowed :: Bool,
    resend_wait_time :: Text,
    is_otp_required :: Bool,
    is_submit_allowed :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data SubDetail = SubDetail
  { current_balance :: Maybe Text,
    last_refreshed :: Maybe Text,
    payment_method :: Text,
    payment_method_type :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data RefreshWalletReq = RefreshWalletReq
  { command :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance ToForm RefreshWalletReq where
  toForm RefreshWalletReq {..} =
    Form $ HM.fromList [("command", [toQueryParam command])]

data RefreshWalletResp = RefreshWalletResp
  { token :: Text,
    linked :: Bool,
    id :: Text,
    current_balance :: Maybe Double,
    wallet :: Text,
    last_refreshed :: Maybe Text,
    sub_details :: [RefreshSubDetail],
    object :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data RefreshSubDetail = RefreshSubDetail
  { current_balance :: Maybe Double,
    last_refreshed :: Maybe Text,
    payment_method :: Text,
    payment_method_type :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
