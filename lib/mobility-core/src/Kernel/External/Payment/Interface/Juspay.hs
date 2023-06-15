{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Payment.Interface.Juspay
  ( module Reexport,
    createOrder,
    orderStatus,
    orderStatusWebhook,
  )
where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.External.Payment.Interface.Types
import Kernel.External.Payment.Juspay.Config as Reexport
import qualified Kernel.External.Payment.Juspay.Flow as Juspay
import qualified Kernel.External.Payment.Juspay.Types as Juspay
import qualified Kernel.External.Payment.Juspay.Webhook as Juspay
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Beckn.Ack
import Servant hiding (throwError)

createOrder ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  CreateOrderReq ->
  m CreateOrderResp
createOrder config req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  Juspay.createOrder url apiKey merchantId (mkCreateOrderReq config.returnUrl req)

mkCreateOrderReq :: BaseUrl -> CreateOrderReq -> Juspay.CreateOrderReq
mkCreateOrderReq returnUrl CreateOrderReq {..} =
  Juspay.CreateOrderReq
    { order_id = orderShortId,
      amount = show amount,
      customer_id = customerId,
      customer_email = customerEmail,
      customer_phone = customerPhone,
      payment_page_client_id = paymentPageClientId,
      action = "paymentPage",
      return_url = showBaseUrl returnUrl,
      description = "Complete your payment",
      first_name = customerFirstName,
      last_name = customerLastName
    }

orderStatus ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  OrderStatusReq ->
  m OrderStatusResp
orderStatus config req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  mkOrderStatusResp <$> Juspay.orderStatus url apiKey merchantId req.orderShortId

mkOrderStatusResp :: Juspay.OrderStatusResp -> OrderStatusResp
mkOrderStatusResp Juspay.OrderData {..} =
  OrderStatusResp
    { orderShortId = order_id,
      transactionUUID = txn_uuid,
      transactionStatusId = status_id,
      transactionStatus = status,
      paymentMethodType = payment_method_type,
      paymentMethod = payment_method,
      respMessage = resp_message,
      respCode = resp_code,
      gatewayReferenceId = gateway_reference_id,
      amount = realToFrac amount,
      currency,
      dateCreated = date_created
    }

orderStatusWebhook ::
  EncFlow m r =>
  PaymentServiceConfig ->
  (OrderStatusResp -> Text -> m AckResponse) ->
  BasicAuthData ->
  Value ->
  m AckResponse
orderStatusWebhook paymentConfig orderStatusHandler authData val = do
  Juspay.orderStatusWebhook paymentConfig (orderStatusHandler . mkOrderStatusResp . (.content.order)) authData val
