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
    mandateNotification,
    mandateExecution,
    mandateRevoke,
    mandatePause,
    mandateResume,
    autoRefund,
    mandateNotificationStatus,
  )
where

import qualified Data.Aeson as A
import Data.Text (pack, replace, toUpper)
import qualified Data.Text as T
import Data.Time (UTCTime (utctDay), addDays)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format
import GHC.Float (double2Int)
import Kernel.External.Encryption
import Kernel.External.Payment.Interface.Types
import Kernel.External.Payment.Juspay.Config as Reexport
import qualified Kernel.External.Payment.Juspay.Flow as Juspay
import qualified Kernel.External.Payment.Juspay.Types as Juspay
import qualified Kernel.External.Payment.Juspay.Webhook as Juspay
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.Ack
import Kernel.Types.Error
import Kernel.Utils.Common (HighPrecMoney, Log, MonadTime, fromMaybeM, getCurrentTime)
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
  orderReq <- mkCreateOrderReq config.returnUrl merchantId req
  Juspay.createOrder url apiKey merchantId orderReq

mandateNotification ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  MandateNotificationReq ->
  m MandateNotificationRes
mandateNotification config req = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  notificationResponse <- Juspay.mandateNotification url apiKey req.mandateId (mkNotificationReq req)
  return $ mkNotificationRes notificationResponse
  where
    mkNotificationRes Juspay.MandateNotificationRes {..} =
      MandateNotificationRes
        { juspayProvidedId = id,
          sourceInfo = castSourceInfo source_info,
          notificationId = object_reference_id,
          providerName = provider_name,
          notificationType = notification_type,
          description,
          status,
          dateCreated = posixSecondsToUTCTime <$> (fromIntegral <$> (readMaybe (T.unpack date_created) :: Maybe Int)),
          lastUpdated = posixSecondsToUTCTime <$> (fromIntegral <$> (readMaybe (T.unpack last_updated) :: Maybe Int))
        }

mandateNotificationStatus ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  NotificationStatusReq ->
  m NotificationStatusResp
mandateNotificationStatus config req = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  notificationStatusResponse <- Juspay.mandateNotificationStatus url apiKey req.notificationId
  return $ mkNotificationStatusRes notificationStatusResponse
  where
    mkNotificationStatusRes Juspay.NotificationStatusResp {..} =
      NotificationStatusResp
        { id,
          sourceObject = source_object,
          sourceObjectId = source_object_id,
          sourceInfo = castSourceInfo source_info,
          objectReferenceId = object_reference_id,
          providerName = provider_name,
          notificationType = notification_type,
          providerResponse =
            ( \pR ->
                Just $
                  ProviderResponse
                    { providerRefId = pR.provider_ref_id,
                      notificationDate = (\date -> Just (posixSecondsToUTCTime $ fromIntegral (read (T.unpack date) :: Int))) =<< pR.notification_date
                    }
            )
              =<< provider_response,
          description,
          status,
          dateCreated = posixSecondsToUTCTime <$> (fromIntegral <$> (readMaybe (T.unpack date_created) :: Maybe Int)),
          lastUpdated = posixSecondsToUTCTime <$> (fromIntegral <$> (readMaybe (T.unpack last_updated) :: Maybe Int))
        }

mandateExecution ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  MandateExecutionReq ->
  m MandateExecutionRes
mandateExecution config req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  executionResponse <- Juspay.mandateExecution url apiKey (mkExecutionReq req merchantId)
  return $ mkExecutionResponse executionResponse
  where
    mkExecutionResponse Juspay.MandateExecutionRes {..} =
      MandateExecutionRes
        { orderId = order_id,
          txnId = txn_id,
          txnUUID = txn_uuid,
          status
        }

mandateRevoke ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  MandateRevokeReq ->
  m MandateRevokeRes
mandateRevoke config req = do
  let url = config.url
  let merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  void $ Juspay.mandateRevoke url apiKey merchantId req.mandateId Juspay.MandateRevokeReq {command = "revoke"}
  return Success

mkCreateOrderReq :: MonadTime m => BaseUrl -> Text -> CreateOrderReq -> m Juspay.CreateOrderReq
mkCreateOrderReq returnUrl clientId CreateOrderReq {..} =
  do
    return
      Juspay.CreateOrderReq
        { order_id = orderShortId,
          amount = show amount,
          customer_id = customerId,
          customer_email = customerEmail,
          customer_phone = customerPhone,
          payment_page_client_id = clientId,
          action = "paymentPage",
          return_url = showBaseUrl returnUrl,
          description = "Complete your payment",
          first_name = customerFirstName,
          last_name = customerLastName,
          mandate_max_amount = show <$> mandateMaxAmount,
          mandate_frequency = mandateFrequency,
          create_mandate = createMandate,
          metadata_mandate_name = if isJust createMandate then Just (toUpper clientId) else Nothing,
          metadata_remarks = ("Amount to be paid now is Rs " <>) . show . double2Int . realToFrac $ amount,
          mandate_start_date = mandateStartDate,
          mandate_end_date = mandateEndDate
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
  case mandate of
    Just justMandate ->
      MandateOrderStatusResp
        { eventName = Nothing,
          orderShortId = order_id,
          transactionUUID = txn_uuid,
          transactionStatusId = status_id,
          transactionStatus = status,
          paymentMethodType = payment_method_type,
          paymentMethod = payment_method,
          respMessage = resp_message,
          respCode = resp_code,
          gatewayReferenceId = gateway_reference_id,
          amount = realToFrac amount,
          currency = currency,
          dateCreated = date_created,
          bankErrorMessage = if bank_error_message == Just "" then Nothing else bank_error_message,
          bankErrorCode = if bank_error_code == Just "" then Nothing else bank_error_code,
          mandateStartDate = posixSecondsToUTCTime <$> (fromIntegral <$> (readMaybe (T.unpack justMandate.start_date) :: Maybe Int)),
          mandateEndDate = posixSecondsToUTCTime <$> (fromIntegral <$> (readMaybe (T.unpack justMandate.end_date) :: Maybe Int)),
          mandateId = justMandate.mandate_id,
          mandateStatus = justMandate.mandate_status,
          mandateFrequency = justMandate.frequency,
          mandateMaxAmount = justMandate.max_amount,
          payerVpa = payer_vpa,
          upi = castUpi <$> upi
        }
    Nothing ->
      OrderStatusResp
        { eventName = Nothing,
          orderShortId = order_id,
          transactionUUID = txn_uuid,
          transactionStatusId = status_id,
          transactionStatus = status,
          paymentMethodType = payment_method_type,
          paymentMethod = payment_method,
          respMessage = resp_message,
          respCode = resp_code,
          gatewayReferenceId = gateway_reference_id,
          amount = realToFrac amount,
          currency = currency,
          bankErrorMessage = if bank_error_message == Just "" then Nothing else bank_error_message,
          bankErrorCode = if bank_error_code == Just "" then Nothing else bank_error_code,
          dateCreated = date_created
        }

castUpi :: Juspay.Upi -> Upi
castUpi Juspay.Upi {..} = Upi {payerApp = payer_app, payerAppName = payer_app_name, txnFlowType = txn_flow_type, payerVpa = payer_vpa}

mkNotificationReq :: MandateNotificationReq -> Juspay.MandateNotificationReq
mkNotificationReq mandateNotificationReq =
  Juspay.MandateNotificationReq
    { command = "pre_debit_notify",
      object_reference_id = mandateNotificationReq.notificationId,
      source_info = Juspay.SourceInfo {amount = show mandateNotificationReq.amount, txn_date = show $ utcTimeToPOSIXSeconds mandateNotificationReq.txnDate},
      description = mandateNotificationReq.description
    }

mkExecutionReq :: MandateExecutionReq -> Text -> Juspay.MandateExecutionReq
mkExecutionReq MandateExecutionReq {..} merchantId =
  Juspay.MandateExecutionReq
    { merchantId,
      mandateId = mandateId,
      mandate = Juspay.MandateInfo {notificationId = notificationId, executionDate = show $ utcTimeToPOSIXSeconds executionDate},
      order = Juspay.MandateOrder {orderId = orderId, orderAmount = show amount, orderCustomerId = customerId},
      format = "json"
    }

mandatePause ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  MandatePauseReq ->
  m ()
mandatePause config req = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  juspayReq <- mkPauseReq req
  Juspay.mandatePause url apiKey req.mandateId juspayReq

mkPauseReq ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  MandatePauseReq ->
  m Juspay.MandatePauseReq
mkPauseReq MandatePauseReq {..} = do
  now <- liftIO getCurrentTime
  return $
    Juspay.MandatePauseReq
      { command = "pause",
        pause_start_date = show $ utcTimeToPOSIXSeconds pauseStartDate,
        pause_end_date = show $ utcTimeToPOSIXSeconds $ fromMaybe (addDaysUtcTime now (365 * 20)) pauseEndDate
      }

mandateResume ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  MandateResumeReq ->
  m ()
mandateResume config req = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  Juspay.mandateResume url apiKey req.mandateId (mkResumeReq req)

mkResumeReq :: MandateResumeReq -> Juspay.MandateResumeReq
mkResumeReq req =
  Juspay.MandateResumeReq
    { command = "resume",
      resume_date = show $ utcTimeToPOSIXSeconds req.resumeDate
    }

addDaysUtcTime :: UTCTime -> Integer -> UTCTime
addDaysUtcTime t x = t {utctDay = addDays x (utctDay t)}

orderStatusWebhook ::
  EncFlow m r =>
  PaymentServiceConfig ->
  (OrderStatusResp -> Text -> m AckResponse) ->
  BasicAuthData ->
  A.Value ->
  m (Maybe OrderStatusResp)
orderStatusWebhook paymentConfig orderStatusHandler authData val = do
  now <- getCurrentTime
  response <- Juspay.orderStatusWebhook paymentConfig (orderStatusHandler . mkWebhookOrderStatusResp now . (\resp -> (resp.event_name, resp.content))) authData val
  return $ mkWebhookOrderStatusResp now <$> response

mkWebhookOrderStatusResp :: UTCTime -> (Juspay.PaymentStatus, Juspay.OrderAndNotificationStatusContent) -> OrderStatusResp
mkWebhookOrderStatusResp now (eventName, Juspay.OrderAndNotificationStatusContent {..}) =
  case (order, mandate, notification, txn) of
    (Just justOrder, Nothing, _, _) ->
      case justOrder.mandate of
        Just justMandate ->
          MandateOrderStatusResp
            { eventName = Just eventName,
              orderShortId = justOrder.order_id,
              transactionUUID = justOrder.txn_uuid,
              transactionStatusId = justOrder.status_id,
              transactionStatus = justOrder.status,
              paymentMethodType = justOrder.payment_method_type,
              paymentMethod = justOrder.payment_method,
              respMessage = justOrder.resp_message,
              respCode = justOrder.resp_code,
              gatewayReferenceId = justOrder.gateway_reference_id,
              amount = realToFrac justOrder.amount,
              bankErrorMessage = if justOrder.bank_error_message == Just "" then Nothing else justOrder.bank_error_message,
              bankErrorCode = if justOrder.bank_error_code == Just "" then Nothing else justOrder.bank_error_code,
              currency = justOrder.currency,
              dateCreated = justOrder.date_created,
              mandateStartDate = posixSecondsToUTCTime <$> (fromIntegral <$> (readMaybe (T.unpack justMandate.start_date) :: Maybe Int)),
              mandateEndDate = posixSecondsToUTCTime <$> (fromIntegral <$> (readMaybe (T.unpack justMandate.end_date) :: Maybe Int)),
              mandateStatus = justMandate.mandate_status,
              mandateId = justMandate.mandate_id,
              mandateFrequency = justMandate.frequency,
              mandateMaxAmount = justMandate.max_amount,
              payerVpa = justOrder.payer_vpa,
              upi = castUpi <$> justOrder.upi
            }
        Nothing ->
          OrderStatusResp
            { eventName = Just eventName,
              orderShortId = justOrder.order_id,
              transactionUUID = justOrder.txn_uuid,
              transactionStatusId = justOrder.status_id,
              transactionStatus = justOrder.status,
              paymentMethodType = justOrder.payment_method_type,
              paymentMethod = justOrder.payment_method,
              respMessage = justOrder.resp_message,
              respCode = justOrder.resp_code,
              gatewayReferenceId = justOrder.gateway_reference_id,
              bankErrorMessage = if justOrder.bank_error_message == Just "" then Nothing else justOrder.bank_error_message,
              bankErrorCode = if justOrder.bank_error_code == Just "" then Nothing else justOrder.bank_error_code,
              amount = realToFrac justOrder.amount,
              currency = justOrder.currency,
              dateCreated = justOrder.date_created
            }
    (Nothing, Just justMandate, _, _) ->
      MandateStatusResp
        { eventName = Just eventName,
          orderShortId = justMandate.order_id,
          status = justMandate.status,
          mandateStartDate = posixSecondsToUTCTime <$> (fromIntegral <$> (readMaybe (T.unpack justMandate.start_date) :: Maybe Int)),
          mandateEndDate = posixSecondsToUTCTime <$> (fromIntegral <$> (readMaybe (T.unpack justMandate.end_date) :: Maybe Int)),
          mandateId = justMandate.mandate_id,
          mandateFrequency = justMandate.frequency,
          mandateMaxAmount = justMandate.max_amount,
          upi = castUpi <$> (justMandate.payment_info >>= (.upi))
        }
    (_, _, Just justNotification, _) ->
      PDNNotificationStatusResp
        { eventName = Just eventName,
          notificationStatus = justNotification.status,
          sourceObject = justNotification.source_object,
          sourceInfo = maybe SourceInfo {txnDate = Just now, sourceAmount = Just 0} castSourceInfo (justNotification.source_info),
          notificationType = justNotification.notification_type,
          juspayProviedId = justNotification.id,
          notificationId = justNotification.object_reference_id
        }
    (_, _, _, Just justTransaction) ->
      OrderStatusResp
        { eventName = Just eventName,
          orderShortId = justTransaction.order_id,
          transactionUUID = justTransaction.txn_uuid,
          transactionStatusId = justTransaction.status_id,
          transactionStatus = justTransaction.status,
          paymentMethodType = Nothing,
          paymentMethod = Nothing,
          respMessage = Nothing,
          respCode = Nothing,
          gatewayReferenceId = Nothing,
          bankErrorMessage = if justTransaction.error_message == Just "" then Nothing else justTransaction.error_message,
          bankErrorCode = if justTransaction.error_code == Just "" then Nothing else justTransaction.error_code,
          amount = realToFrac justTransaction.txn_amount,
          currency = justTransaction.currency,
          dateCreated = Nothing
        }
    (_, _, Nothing, _) -> BadStatusResp

castSourceInfo :: Juspay.SourceInfo -> SourceInfo
castSourceInfo source_info =
  SourceInfo
    { txnDate = posixSecondsToUTCTime <$> (fromIntegral <$> (readMaybe (T.unpack source_info.txn_date) :: Maybe Int)),
      sourceAmount = readMaybe (T.unpack source_info.amount) :: Maybe HighPrecMoney
    }

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
mkOfferListReq OfferListReq {..} =
  Juspay.OfferListReq
    { order = mkOfferOrder order planId registrationDate dutyDate paymentMode numOfRides,
      payment_method_info = [],
      customer = mkOfferCustomer <$> customer,
      offer_code = Nothing
    }

mkOfferOrder :: OfferOrder -> Text -> UTCTime -> UTCTime -> Text -> Int -> Juspay.OfferOrder
---- add duty day and payment mode respectively in holes ----
mkOfferOrder OfferOrder {..} planId registrationDate dutyDate paymentMode numOfRides =
  Juspay.OfferOrder
    { order_id = orderId,
      amount = show amount,
      currency,
      udf1 = replace "-" "_" planId,
      udf2 = pack $ formatTime defaultTimeLocale "%d_%m_%y" registrationDate,
      udf3 = paymentMode,
      udf4 = pack $ formatTime defaultTimeLocale "%d_%m_%y" dutyDate,
      udf5 = show numOfRides
    }

mkOfferCustomer :: OfferCustomer -> Juspay.OfferCustomer
mkOfferCustomer OfferCustomer {..} = Juspay.OfferCustomer {id = customerId, email, mobile}

buildOfferListResp :: (MonadThrow m, Log m) => Juspay.OfferListResp -> m OfferListResp
buildOfferListResp resp = do
  bestOfferCombination <- buildBestOfferCombination `mapM` (listToMaybe resp.best_offer_combinations)
  let offerResp = filter (\offer -> offer.status == ELIGIBLE) $ mkOfferResp <$> resp.offers
  pure OfferListResp {..}

mkOfferResp :: Juspay.OfferResp -> OfferResp
mkOfferResp Juspay.OfferResp {..} = do
  OfferResp
    { offerId = offer_id,
      status,
      offerDescription = mkOfferDescription offer_description,
      orderAmount = read $ T.unpack order_breakup.final_order_amount,
      finalOrderAmount = read $ T.unpack order_breakup.final_order_amount,
      discountAmount = read $ T.unpack order_breakup.discount_amount
    }

mkOfferDescription :: Juspay.OfferDescription -> OfferDescription
mkOfferDescription Juspay.OfferDescription {..} = OfferDescription {sponsoredBy = sponsored_by, ..}

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
  juspayResp <- Juspay.offerApply url apiKey merchantId juspayReq
  buildOfferApplyResp juspayResp

mkOfferApplyReq :: Text -> OfferApplyReq -> Juspay.OfferApplyReq
mkOfferApplyReq merchantId OfferApplyReq {..} = do
  let order =
        Juspay.OfferApplyOrder
          { amount = show amount,
            currency,
            merchant_id = Just merchantId,
            order_type = Just "ORDER_PAYMENT",
            udf1 = replace "-" "_" planId,
            udf2 = pack $ formatTime defaultTimeLocale "%d_%m_%y" registrationDate,
            udf3 = paymentMode,
            udf4 = pack $ formatTime defaultTimeLocale "%d_%m_%y" dutyDate,
            udf5 = show numOfRides,
            payment_channel = Just "WEB"
          }
  Juspay.OfferApplyReq
    { txn_id = txnId,
      customer = Juspay.OfferApplyCustomer {id = customerId},
      offers,
      order,
      payment_method_info = Just $ Juspay.OfferApplyPaymentMethodInfo Juspay.UPI Nothing Nothing Nothing Nothing Nothing Nothing
    }

buildOfferApplyResp :: (MonadThrow m, Log m) => Juspay.OfferApplyResp -> m OfferApplyResp
buildOfferApplyResp resp = do
  offers <- forM resp.offers $ \offer -> do
    finalOrderAmount <- parseMoney offer.order_breakup.final_order_amount "final_order_amount"
    pure
      OfferApplyRespItem
        { finalOrderAmount,
          offerId = offer.offer_id
        }
  pure OfferApplyResp {offers}

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
  void $ Juspay.offerNotify url apiKey merchantId req.mandateId juspayReq
  pure Success

mkOfferNotifyReq :: Text -> OfferNotifyReq -> Juspay.OfferNotifyReq
mkOfferNotifyReq merchantId OfferNotifyReq {..} = do
  Juspay.OfferNotifyReq
    { order_id = orderShortId,
      txn_id = transactionUUID,
      merchant_id = merchantId,
      txn_status = transactionStatus,
      offers = offers <&> (\OfferNotifyOffer {offerId, status} -> Juspay.OfferNotifyOffer {offer_id = offerId, status})
    }

autoRefund ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  AutoRefundReq ->
  m () ---- to do refund -----
autoRefund config req = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  Juspay.autoRefund url apiKey req.orderId (mkAutoPayRequest req)
  where
    mkAutoPayRequest request =
      Juspay.AutoRefundReq
        { unique_request_id = request.requestId,
          amount = request.amount
        }
