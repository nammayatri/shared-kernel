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
import Kernel.External.Payment.Interface.Types (RegisterMandateReq, RegisterMandateResp)
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
    :> Header "x-merchantid" Text
    :> ReqBody '[JSON] CreateOrderReq
    :> Post '[JSON] CreateOrderResp

type RegisterMandateAPI =
  "txns"
    :> BasicAuth "username-password" BasicAuthData
    :> ReqBody '[FormUrlEncoded] RegisterMandateReq
    :> Post '[JSON] RegisterMandateResp

registerMandate ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  RegisterMandateReq ->
  m RegisterMandateResp
registerMandate url apiKey req = do
  let eulerClient = Euler.client (Proxy @RegisterMandateAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient basicAuthData req) "register-mandate" (Proxy @RegisterMandateAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call register mandate API: " <> show err)

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
  callAPI url (eulerClient basicAuthData (Just merchantId) req) "create-order" (Proxy @CreateOrderAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call create order API: " <> show err)

type OrderStatusAPI =
  "orders"
    :> Capture "orderId" Text
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> MandatoryQueryParam "version" Text
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
  callAPI url (eulerClient orderId basicAuthData (Just merchantId) version) "order-status" (Proxy @OrderStatusAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call order status API: " <> show err)

getCurrentDate :: MonadFlow m => m Text
getCurrentDate = do
  currentTime <- getCurrentTime
  let dateFormat = "%Y-%m-%d"
      formattedDate = formatTime defaultTimeLocale dateFormat currentTime
  return $ encodeToText formattedDate

type MandateNotificationAPI =
  "mandates"
    :> Capture "merchantId" Text
    :> BasicAuth "username-password" BasicAuthData
    :> ReqBody '[FormUrlEncoded] MandateNotificationReq
    :> Post '[JSON] MandateNotificationRes

mandateNotification ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  MandateNotificationReq ->
  m MandateNotificationRes
mandateNotification url apiKey merchantId req = do
  let eulerClient = Euler.client (Proxy @MandateNotificationAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient merchantId basicAuthData req) "mandate-notification" (Proxy @MandateNotificationAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call mandate notification API: " <> show err)

type MandateExecutionAPI =
  "txns"
    :> BasicAuth "username-password" BasicAuthData
    :> ReqBody '[FormUrlEncoded] MandateExecutionReq
    :> Post '[JSON] MandateExecutionRes

mandateExecution ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  MandateExecutionReq ->
  m MandateExecutionRes
mandateExecution url apiKey req = do
  let eulerClient = Euler.client (Proxy @MandateExecutionAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient basicAuthData req) "mandate-notification" (Proxy @MandateExecutionAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call mandate Execution API: " <> show err)

type MandateRevokeAPI =
  "mandates"
    :> Header "x-merchantid" Text
    :> BasicAuth "username-password" BasicAuthData
    :> Capture "mandateId" Text
    :> ReqBody '[FormUrlEncoded] MandateRevokeReq
    :> Post '[JSON] MandateRevokeRes

mandateRevoke ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  MandateRevokeReq ->
  m MandateRevokeRes
mandateRevoke url apiKey merchantId mandateId req = do
  let eulerClient = Euler.client (Proxy @MandateRevokeAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient (Just merchantId) basicAuthData mandateId req) "mandate-Revoke" (Proxy @MandateRevokeAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call mandate Revoke API: " <> show err)
