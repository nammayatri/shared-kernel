{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Payout.Juspay.Flow where

import qualified Data.Text.Encoding as DT
import EulerHS.Types as Euler
import qualified Kernel.External.Payout.Interface.Types as Payout
import Kernel.External.Payout.Juspay.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (fromEitherM)
import Kernel.Utils.Servant.Client
import Servant hiding (throwError)

mkBasicAuthData :: Text -> BasicAuthData
mkBasicAuthData apiKey =
  BasicAuthData
    { basicAuthUsername = DT.encodeUtf8 apiKey,
      basicAuthPassword = ""
    }

type CreatePayoutOrderAPI =
  "payout" :> "merchant" :> "v1" :> "orders"
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> ReqBody '[JSON] CreatePayoutOrderReq
    :> Post '[JSON] CreatePayoutOrderResp

createPayoutOrder ::
  (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) =>
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  CreatePayoutOrderReq ->
  m CreatePayoutOrderResp
createPayoutOrder url apiKey merchantId mRoutingId req = do
  let proxy = Proxy @CreatePayoutOrderAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) (Just merchantId) mRoutingId req
  callAPI url eulerClient "create-payout-order" proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call create payout order API: " <> show err)

type PayoutOrderStatusAPI =
  "payout" :> "merchant" :> "v1" :> "orders"
    :> Capture "orderId" Text
    :> BasicAuth "username-password" BasicAuthData
    :> QueryParam "expand" Payout.Expand
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> Get '[JSON] PayoutOrderStatusResp

payoutOrderStatus ::
  (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) =>
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  Text ->
  Maybe Payout.Expand ->
  m PayoutOrderStatusResp
payoutOrderStatus url apiKey merchantId mRoutingId orderId mbExpand = do
  let proxy = Proxy @PayoutOrderStatusAPI
      eulerClient = Euler.client proxy orderId (mkBasicAuthData apiKey) mbExpand (Just merchantId) mRoutingId
  callAPI url eulerClient "payout-order-status" proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call payout order status API: " <> show err)
