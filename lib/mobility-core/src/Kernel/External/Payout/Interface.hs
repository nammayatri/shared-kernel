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
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

createPayoutOrder ::
  ( EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl]
  ) =>
  PayoutServiceConfig ->
  CreatePayoutOrderReq ->
  m CreatePayoutOrderResp
createPayoutOrder serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.createPayoutOrder cfg req

payoutOrderStatus ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PayoutServiceConfig ->
  PayoutOrderStatusReq ->
  m PayoutOrderStatusResp
payoutOrderStatus serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.payoutOrderStatus cfg req.orderId req.mbExpand
