{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Juspay.Types.Fulfillment where

import Kernel.External.Payment.Juspay.Types.CreateOrder (SplitSettlementDetails)
import Kernel.Prelude
import Servant (ToHttpApiData (..))
import Web.FormUrlEncoded

data FulfillmentReq = FulfillmentReq
  { fulfillment_status :: Text,
    fulfillment_command :: Text,
    split_settlement_details :: Maybe SplitSettlementDetails
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToForm FulfillmentReq where
  toForm FulfillmentReq {..} =
    toForm (params ++ maybe [] splitSettleParam split_settlement_details)
    where
      params :: [(Text, Text)]
      params =
        [ ("fulfillment_status", toQueryParam fulfillment_status),
          ("fulfillment_command", toQueryParam fulfillment_command)
        ]

      splitSettleParam :: SplitSettlementDetails -> [(Text, Text)]
      splitSettleParam details =
        [("split_settlement_details", toQueryParam details)]

data FulfillmentResp = FulfillmentResp
  { order_id :: Maybe Text,
    status :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
