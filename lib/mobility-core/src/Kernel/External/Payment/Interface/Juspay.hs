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
    offerList,
    offerApply,
    offerNotify,
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
import Kernel.Types.Error
import Kernel.Utils.Common (HighPrecMoney, Log, fromMaybeM)
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
  m (Maybe Juspay.OrderStatusContent)
orderStatusWebhook paymentConfig orderStatusHandler authData val = do
  Juspay.orderStatusWebhook paymentConfig (orderStatusHandler . mkOrderStatusResp . (.content.order)) authData val

offerList ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  OfferListReq ->
  m OfferListResp
offerList config req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  let juspayReq = mkOfferListReq req
  juspayResp <- Juspay.offerList url apiKey merchantId juspayReq
  buildOfferListResp juspayResp

mkOfferListReq :: OfferListReq -> Juspay.OfferListReq
mkOfferListReq OfferListReq {..} = do
  Juspay.OfferListReq
    { order = mkOfferOrder order,
      payment_method_info = [],
      customer = mkOfferCustomer customer,
      offer_code = Nothing
    }

mkOfferOrder :: OfferOrder -> Juspay.OfferOrder
mkOfferOrder OfferOrder {..} = Juspay.OfferOrder {order_id = orderId, amount = show amount, currency}

mkOfferCustomer :: OfferCustomer -> Juspay.OfferCustomer
mkOfferCustomer OfferCustomer {..} = Juspay.OfferCustomer {id = customerId, email, mobile}

offerApply ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  OfferApplyReq ->
  m OfferApplyResp
offerApply config req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  let juspayReq = mkOfferApplyReq merchantId req
  Juspay.offerApply url apiKey merchantId juspayReq

mkOfferApplyReq :: Text -> OfferApplyReq -> Juspay.OfferApplyReq
mkOfferApplyReq merchantId OfferApplyReq {..} = do
  let order =
        Juspay.OfferApplyOrder
          { order_id = orderShortId,
            amount = show amount,
            currency,
            merchant_id = Just merchantId,
            order_type = Just "ORDER_PAYMENT",
            udf1 = Nothing,
            payment_channel = Just "WEB" -- is it correct?
          }
  let payment_method_info =
        Juspay.OfferApplyPaymentMethodInfo
          { payment_method_type = Juspay.NB, -- is it correct?
            payment_method_reference = Nothing,
            payment_method = Nothing,
            card_type = Nothing,
            card_sub_type = Nothing,
            bank_code = Nothing,
            card_bin = Nothing
          }
  Juspay.OfferApplyReq
    { customer = Juspay.OfferApplyCustomer {id = customerId},
      offers,
      order,
      payment_method_info
    }

buildOfferListResp :: (MonadThrow m, Log m) => Juspay.OfferListResp -> m OfferListResp
buildOfferListResp resp = do
  bestOfferCombination <- buildBestOfferCombination `mapM` (listToMaybe resp.best_offer_combinations)
  let offerResp = mkOfferResp <$> resp.offers
  pure OfferListResp {..}

mkOfferResp :: Juspay.OfferResp -> OfferResp
mkOfferResp Juspay.OfferResp {..} = do
  OfferResp
    { offerId = offer_id,
      status,
      offerDescription = offer_description.title
    }

buildBestOfferCombination :: (MonadThrow m, Log m) => Juspay.BestOfferCombination -> m BestOfferCombination
buildBestOfferCombination combination = do
  offers <- buildBestOfferCombinationOffer `mapM` combination.offers
  orderBreakup <- buildOrderBreakup combination.order_breakup
  pure BestOfferCombination {..}

buildBestOfferCombinationOffer :: (MonadThrow m, Log m) => Juspay.BestOfferCombinationOffer -> m BestOfferCombinationOffer
buildBestOfferCombinationOffer Juspay.BestOfferCombinationOffer {..} = do
  cashbackAmount <- parseMoney cashback_amount "cashback_amount"
  discountAmount <- parseMoney discount_amount "discount_amount"
  merchantDiscountAmount <- parseMoney merchant_discount_amount "merchant_discount_amount"
  totalOfferedAmount <- parseMoney total_offered_amount "total_offered_amount"
  pure $ BestOfferCombinationOffer {offerId = offer_id, ..}

buildOrderBreakup :: (MonadThrow m, Log m) => Juspay.OrderBreakup -> m OrderBreakup
buildOrderBreakup Juspay.OrderBreakup {..} = do
  orderAmount <- parseMoney order_amount "order_amount"
  finalOrderAmount <- parseMoney final_order_amount "final_order_amount"
  discountAmount <- parseMoney discount_amount "discount_amount"
  merchantDiscountAmount <- parseMoney merchant_discount_amount "merchant_discount_amount"
  cashbackAmount <- parseMoney cashback_amount "cashback_amount"
  offerAmount <- parseMoney offer_amount "offer_amount"
  pure $ OrderBreakup {..}

parseMoney :: (MonadThrow m, Log m) => Text -> Text -> m HighPrecMoney
parseMoney field desc = do
  readMaybe (show field) & fromMaybeM (InternalError $ "Couldn't parse " <> desc)

offerNotify ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  OfferNotifyReq ->
  m OfferNotifyResp
offerNotify config req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  let juspayReq = mkOfferNotifyReq merchantId req
  Juspay.offerNotify url apiKey merchantId juspayReq

mkOfferNotifyReq :: Text -> OfferNotifyReq -> Juspay.OfferNotifyReq
mkOfferNotifyReq merchantId OfferNotifyReq {..} = do
  Juspay.OfferNotifyReq
    { order_id = orderShortId,
      txn_id = transactionUUID,
      merchant_id = merchantId,
      txn_status = transactionStatus,
      offers = offers <&> (\OfferNotifyOffer {offerId, status} -> Juspay.OfferNotifyOffer {offer_id = offerId, status})
    }
