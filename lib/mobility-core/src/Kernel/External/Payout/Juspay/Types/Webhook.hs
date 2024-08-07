{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kernel.External.Payout.Juspay.Types.Webhook where

import Data.Aeson
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import Kernel.Prelude
import Kernel.Utils.JSON

data PayoutWebhookReq = PayoutWebhookReq
  { id :: Text,
    label :: Maybe Text,
    value :: Maybe Text,
    category :: Maybe Text,
    info :: PayoutInfo
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PayoutInfo = PayoutInfo
  { id :: Text,
    status :: Payout.PayoutOrderStatus,
    _type :: Maybe Text,
    merchantOrderId :: Text,
    amount :: Double,
    merchantCustomerId :: Maybe Text,
    createdAt :: Maybe Text,
    updatedAt :: Maybe Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON PayoutInfo where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON PayoutInfo where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
