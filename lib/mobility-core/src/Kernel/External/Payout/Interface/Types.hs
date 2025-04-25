{-# LANGUAGE DerivingStrategies #-}
{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Kernel.External.Payout.Interface.Types
  ( module Kernel.External.Payout.Interface.Types,
    module Reexport,
  )
where

import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import qualified Kernel.External.Payout.Juspay.Config as Juspay
import Kernel.External.Payout.Juspay.Types as Reexport (Fulfillment (..), PayoutOrderStatus (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.Common hiding (Currency)
import Servant.API (ToHttpApiData (..))

data PayoutServiceConfig = JuspayConfig Juspay.JuspayConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data OrderStatusPayoutResp
  = OrderStatusPayoutResp
      { payoutOrderId :: Text,
        payoutStatus :: PayoutOrderStatus,
        orderType :: Maybe Text,
        merchantCustomerId :: Maybe Text,
        amount :: HighPrecMoney,
        createdAt :: Maybe Text,
        updatedAt :: Maybe Text
      }
  | BadStatusResp
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreatePayoutOrderReq = CreatePayoutOrderReq
  { orderId :: Text,
    amount :: HighPrecMoney,
    customerPhone :: Text,
    customerEmail :: Text,
    customerId :: Text,
    orderType :: Text,
    remark :: Text,
    customerName :: Text,
    customerVpa :: Text,
    isDynamicWebhookRequired :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreatePayoutOrderResp = CreatePayoutOrderResp
  { orderId :: Text,
    status :: PayoutOrderStatus,
    orderType :: Maybe Text,
    udf1 :: Maybe Text,
    udf2 :: Maybe Text,
    udf3 :: Maybe Text,
    udf4 :: Maybe Text,
    udf5 :: Maybe Text,
    amount :: HighPrecMoney,
    refunds :: Maybe [Text],
    payments :: Maybe [Text],
    fulfillments :: Maybe [Fulfillment],
    customerId :: Maybe Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Expand = ExpandFulfillment | ExpandPayment | ExpandRefund
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema, Ord, Read)

$(mkBeamInstancesForEnum ''Expand)

derivePersistField "Expand"

instance ToHttpApiData Expand where
  toUrlPiece ExpandFulfillment = "fulfillment"
  toUrlPiece ExpandPayment = "payment"
  toUrlPiece ExpandRefund = "refund"

data PayoutOrderStatusReq = PayoutOrderStatusReq
  { orderId :: Text,
    mbExpand :: Maybe Expand,
    personId :: Maybe Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type PayoutOrderStatusResp = CreatePayoutOrderResp
