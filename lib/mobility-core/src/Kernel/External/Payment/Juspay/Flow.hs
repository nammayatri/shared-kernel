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
import qualified Kernel.External.Payment.Juspay.Types.Offer as Offer
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

type OfferListAPI =
  "v1" -- is this required?
    :> "offers"
    :> "list"
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> ReqBody '[JSON] Offer.OfferListReq
    :> Post '[JSON] Offer.OfferListResp

offerList ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Offer.OfferListReq ->
  m Offer.OfferListResp
offerList url apiKey merchantId req = do
  let eulerClient = Euler.client (Proxy @OfferListAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient basicAuthData (Just merchantId) req) "offer-list" (Proxy @OfferListAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call offer list API: " <> show err)

type OfferApplyAPI =
  "merchant"
    :> "offers"
    :> Capture "orderId" Text
    :> "apply"
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> ReqBody '[JSON] Offer.OfferApplyReq
    :> Post '[JSON] Offer.OfferApplyResp

offerApply ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Offer.OfferApplyReq ->
  m Offer.OfferApplyResp
offerApply url apiKey merchantId req = do
  let eulerClient = Euler.client (Proxy @OfferApplyAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient req.order.order_id basicAuthData (Just merchantId) req) "offer-apply" (Proxy @OfferApplyAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call offer apply API: " <> show err)

type OfferNotifyAPI =
  "merchant"
    :> "offers"
    :> Capture "orderId" Text
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> ReqBody '[JSON] Offer.OfferNotifyReq
    :> Post '[JSON] Offer.OfferNotifyResp

offerNotify ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Offer.OfferNotifyReq ->
  m Offer.OfferNotifyResp
offerNotify url apiKey merchantId req = do
  let eulerClient = Euler.client (Proxy @OfferNotifyAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient req.order_id basicAuthData (Just merchantId) req) "offer-notify" (Proxy @OfferNotifyAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call offer notify API: " <> show err)
