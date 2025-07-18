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
import Kernel.Utils.Common (CallAPI', callAPI, encodeToText, fromEitherM)
import Servant hiding (throwError)

-- https://docs.juspay.in/payment-page/ios/base-sdk-integration/order-status-api

type CreateCustomerAPI =
  "customers"
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> ReqBody '[JSON] CreateCustomerRequest
    :> Post '[JSON] CreateCustomerResp

createCustomer ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  CreateCustomerRequest ->
  m CreateCustomerResp
createCustomer url apiKey merchantId mRoutingId req = do
  let proxy = Proxy @CreateCustomerAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) (Just merchantId) mRoutingId req
  callJuspayAPI url eulerClient "create-customer" proxy

type GetCustomerAPI =
  "v2" :> "customers"
    :> Capture "id" CustomerId
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> ReqBody '[FormUrlEncoded] GetCustomerReq
    :> Post '[JSON] CreateCustomerResp

getCustomer ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  CustomerId ->
  GetCustomerReq ->
  m CreateCustomerResp
getCustomer url apiKey merchantId mRoutingId customerId req = do
  let proxy = Proxy @GetCustomerAPI
      eulerClient = Euler.client proxy customerId (mkBasicAuthData apiKey) (Just merchantId) mRoutingId req
  callJuspayAPI url eulerClient "get-customer" proxy

type CreateOrderAPI =
  "session"
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> ReqBody '[JSON] CreateOrderReq
    :> Post '[JSON] CreateOrderResp

createOrder ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  CreateOrderReq ->
  m CreateOrderResp
createOrder url apiKey merchantId mRoutingId req = do
  let proxy = Proxy @CreateOrderAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) (Just merchantId) mRoutingId req
  callJuspayAPI url eulerClient "create-order" proxy

type OrderStatusAPI =
  "orders"
    :> Capture "orderId" Text
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> MandatoryQueryParam "version" Text
    :> Get '[JSON] OrderStatusResp

orderStatus ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  Text ->
  m OrderStatusResp
orderStatus url apiKey merchantId mRoutingId orderId = do
  version <- getCurrentDate
  let proxy = Proxy @OrderStatusAPI
      eulerClient = Euler.client proxy orderId (mkBasicAuthData apiKey) (Just merchantId) mRoutingId version
  callJuspayAPI url eulerClient "order-status" proxy

getCurrentDate :: MonadFlow m => m Text
getCurrentDate = do
  currentTime <- getCurrentTime
  let dateFormat = "%Y-%m-%d"
      formattedDate = formatTime defaultTimeLocale dateFormat currentTime
  return $ encodeToText formattedDate

type OrderUpdateAPI =
  "orders"
    :> Capture "orderId" Text
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> ReqBody '[FormUrlEncoded] OrderUpdateReq
    :> Post '[JSON] OrderUpdateResp

updateOrder ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  OrderUpdateReq ->
  m OrderUpdateResp
updateOrder url apiKey merchantId orderId mRoutingId req = do
  let proxy = Proxy @OrderUpdateAPI
      eulerClient = Euler.client proxy orderId (mkBasicAuthData apiKey) (Just merchantId) mRoutingId req
  callJuspayAPI url eulerClient "update-order" proxy

type OfferListAPI =
  "offers"
    :> "list"
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> ReqBody '[JSON] Offer.OfferListReq
    :> Post '[JSON] Offer.OfferListResp

offerList ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  Offer.OfferListReq ->
  m Offer.OfferListResp
offerList url apiKey merchantId mRoutingId req = do
  let proxy = Proxy @OfferListAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) (Just merchantId) mRoutingId req
  callJuspayAPI url eulerClient "offer-list" proxy

type OfferApplyAPI =
  "offers"
    :> "apply"
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> ReqBody '[JSON] Offer.OfferApplyReq
    :> Post '[JSON] Offer.OfferApplyResp

offerApply ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  Offer.OfferApplyReq ->
  m Offer.OfferApplyResp
offerApply url apiKey merchantId mRoutingId req = do
  let proxy = Proxy @OfferApplyAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) (Just merchantId) mRoutingId req
  callJuspayAPI url eulerClient "offer-apply" proxy

type OfferNotifyAPI =
  "merchant"
    :> "offers"
    :> Capture "mandateId" Text
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> ReqBody '[JSON] Offer.OfferNotifyReq
    :> Post '[JSON] Offer.OfferNotifyResp

offerNotify ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  Text ->
  Offer.OfferNotifyReq ->
  m Offer.OfferNotifyResp
offerNotify url apiKey merchantId mRoutingId mandateId req = do
  let proxy = Proxy @OfferNotifyAPI
      eulerClient = Euler.client proxy mandateId (mkBasicAuthData apiKey) (Just merchantId) mRoutingId req
  callJuspayAPI url eulerClient "offer-notify" proxy

callJuspayAPI :: CallAPI' m api res res
callJuspayAPI url eulerClient description proxy = do
  callAPI url eulerClient description proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call " <> description <> " API: " <> show err)

mkBasicAuthData :: Text -> BasicAuthData
mkBasicAuthData apiKey =
  BasicAuthData
    { basicAuthUsername = DT.encodeUtf8 apiKey,
      basicAuthPassword = ""
    }

type MandateNotificationAPI =
  "mandates"
    :> Capture "mandateId" Text
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> ReqBody '[FormUrlEncoded] MandateNotificationReq
    :> Post '[JSON] MandateNotificationRes

mandateNotification ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  MandateNotificationReq ->
  m MandateNotificationRes
mandateNotification url apiKey mandateId merchantId mRoutingId req = do
  let eulerClient = Euler.client (Proxy @MandateNotificationAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient mandateId basicAuthData (Just merchantId) mRoutingId req) "mandate-notification" (Proxy @MandateNotificationAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call mandate notification API: " <> show err)

type NotificationStatusAPI =
  "notifications"
    :> Capture "object_reference_id" Text
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> Get '[JSON] NotificationStatusResp

mandateNotificationStatus ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  m NotificationStatusResp
mandateNotificationStatus url apiKey object_reference_id merchantId mRoutingId = do
  let eulerClient = Euler.client (Proxy @NotificationStatusAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient object_reference_id basicAuthData (Just merchantId) mRoutingId) "mandate-notification-status" (Proxy @NotificationStatusAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call mandate notification status API: " <> show err)

type MandateExecutionAPI =
  "txns"
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> ReqBody '[FormUrlEncoded] MandateExecutionReq
    :> Post '[JSON] MandateExecutionRes

mandateExecution ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  MandateExecutionReq ->
  m MandateExecutionRes
mandateExecution url apiKey merchantId mRoutingId req = do
  let eulerClient = Euler.client (Proxy @MandateExecutionAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient basicAuthData (Just merchantId) mRoutingId req) "mandate-execution" (Proxy @MandateExecutionAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call mandate Execution API: " <> show err)

type MandateRevokeAPI =
  "mandates"
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
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
  Maybe Text ->
  Text ->
  MandateRevokeReq ->
  m MandateRevokeRes
mandateRevoke url apiKey merchantId mRoutingId mandateId req = do
  let eulerClient = Euler.client (Proxy @MandateRevokeAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient (Just merchantId) mRoutingId basicAuthData mandateId req) "mandate-Revoke" (Proxy @MandateRevokeAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call mandate Revoke API: " <> show err)

type MandatePauseAPI =
  "mandates"
    :> Capture "mandateId" Text
    :> BasicAuth "username-password" BasicAuthData
    :> ReqBody '[FormUrlEncoded] MandatePauseReq
    :> Post '[JSON] ()

mandatePause ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  MandatePauseReq ->
  m ()
mandatePause url apiKey mandateId req = do
  let eulerClient = Euler.client (Proxy @MandatePauseAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient mandateId basicAuthData req) "mandate-pause" (Proxy @MandatePauseAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call mandate pause API: " <> show err)

type MandateResumeAPI =
  "mandates"
    :> Capture "mandateId" Text
    :> BasicAuth "username-password" BasicAuthData
    :> ReqBody '[FormUrlEncoded] MandateResumeReq
    :> Post '[JSON] ()

mandateResume ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  MandateResumeReq ->
  m ()
mandateResume url apiKey mandateId req = do
  let eulerClient = Euler.client (Proxy @MandateResumeAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient mandateId basicAuthData req) "mandate-resume" (Proxy @MandateResumeAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call mandate resume API: " <> show err)

type AutoRefundAPI =
  "orders"
    :> Capture "orderId" Text
    :> "refunds"
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> ReqBody '[FormUrlEncoded] AutoRefundReq
    :> Post '[JSON] AutoRefundResp

autoRefund ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  AutoRefundReq ->
  m AutoRefundResp
autoRefund url apiKey orderId merchantId mRoutingId req = do
  let eulerClient = Euler.client (Proxy @AutoRefundAPI)
  let basicAuthData =
        BasicAuthData
          { basicAuthUsername = DT.encodeUtf8 apiKey,
            basicAuthPassword = ""
          }
  callAPI url (eulerClient orderId basicAuthData (Just merchantId) mRoutingId req) "order-refund" (Proxy @AutoRefundAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call order refund API: " <> show err)

type VerifyVPAAPI =
  "v2"
    :> "upi"
    :> "verify-vpa"
    :> BasicAuth "username-password" BasicAuthData
    :> Header "x-merchantid" Text
    :> Header "x-routing-id" Text
    :> ReqBody '[JSON] VerifyVPAReq
    :> Post '[JSON] VerifyVPAResp

verifyVPA ::
  (Metrics.CoreMetrics m, MonadFlow m) =>
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  VerifyVPAReq ->
  m VerifyVPAResp
verifyVPA url apiKey merchantId mRoutingId req = do
  let eulerClient = Euler.client (Proxy @VerifyVPAAPI) (mkBasicAuthData apiKey) (Just merchantId) mRoutingId req
  callAPI url eulerClient "verify-vpa" (Proxy @VerifyVPAAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call verify VPA API: " <> show err)
