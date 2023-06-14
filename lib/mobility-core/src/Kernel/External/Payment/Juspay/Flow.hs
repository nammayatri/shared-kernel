{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Payment.Juspay.Flow where

import qualified Data.Text.Encoding as DT
import Data.Time.Format
import EulerHS.Types as Euler
import Kernel.External.Payment.Juspay.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (callAPI, encodeToText, fromEitherM)
import Servant hiding (throwError)

-- https://docs.juspay.in/payment-page/ios/base-sdk-integration/order-status-api

type CreateOrderAPI =
  "session"
    :> BasicAuth "username-password" BasicAuthData
    :> MandatoryQueryParam "x-merchantid" Text
    :> ReqBody '[JSON] CreateOrderReq
    :> Post '[JSON] CreateOrderResp

createOrder ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  CreateOrderReq ->
  m CreateOrderResp
createOrder url apiKey merchantId req = do
  let eulerClient = Euler.client (Proxy @CreateOrderAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient basicAuthData merchantId req) "create-order" (Proxy @CreateOrderAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call create order API: " <> show err)

type OrderStatusAPI =
  "orders"
    :> Capture "orderId" Text
    :> BasicAuth "username-password" BasicAuthData
    :> MandatoryQueryParam "version" Text
    :> MandatoryQueryParam "x-merchantid" Text
    :> Get '[JSON] OrderStatusResp

orderStatus ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  m OrderStatusResp
orderStatus url apiKey merchantId orderId = do
  version <- getCurrentDate
  let eulerClient = Euler.client (Proxy @OrderStatusAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient orderId basicAuthData version merchantId) "order-status" (Proxy @OrderStatusAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call order status API: " <> show err)

getCurrentDate :: MonadFlow m => m Text
getCurrentDate = do
  currentTime <- getCurrentTime
  let dateFormat = "%Y-%m-%d"
      formattedDate = formatTime defaultTimeLocale dateFormat currentTime
  return $ encodeToText formattedDate
