{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.External.Payout.Interface
  ( module Reexport,
    module Kernel.External.Payout.Interface,
  )
where

import qualified Kernel.External.Payout.Interface.Juspay as Juspay
import Kernel.External.Payout.Interface.Types as Reexport
import Kernel.External.Payout.Types as Reexport
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

createPayoutOrder ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl]
  ) =>
  PayoutServiceConfig ->
  Maybe Text ->
  CreatePayoutOrderReq ->
  m CreatePayoutOrderResp
createPayoutOrder serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> Juspay.createPayoutOrder cfg mRoutingId req

payoutOrderStatus ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PayoutServiceConfig ->
  Maybe Text ->
  PayoutOrderStatusReq ->
  m PayoutOrderStatusResp
payoutOrderStatus serviceConfig mRoutingId req = case serviceConfig of
  JuspayConfig cfg -> Juspay.payoutOrderStatus cfg req.orderId mRoutingId req.mbExpand
