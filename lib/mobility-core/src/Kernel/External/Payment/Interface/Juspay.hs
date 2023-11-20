{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

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
import qualified EulerHS.Prelude as E
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
                      notificationDate = (\date -> posixSecondsToUTCTime <$> (fromIntegral <$> (readMaybe (T.unpack date) :: Maybe Int))) =<< pR.notification_date,
                      responseCode = pR.provider_response_code,
                      responseMessage = pR.provider_response_message
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
          mandate_end_date = mandateEndDate,
          options_get_upi_deep_links = optionsGetUpiDeepLinks,
          metadata_expiry_in_mins = metadataExpiryInMins,
          metadata_gateway_reference_id = metadataGatewayReferenceId
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
          txnId = txn_id,
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
          upi = castUpi <$> upi,
          refunds = maybe [] mkRefundsData refunds,
          amountRefunded = realToFrac <$> amount_refunded
        }
    Nothing -> do
      let (isRetriedOrder, retargetPaymentLink, retargetPaymentLinkExpiry, isRetargetedOrder) = parseRetargetAndRetryData metadata links additional_info
      OrderStatusResp
        { eventName = Nothing,
          orderShortId = order_id,
          transactionUUID = txn_uuid,
          txnId = txn_id,
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
          dateCreated = date_created,
          refunds = maybe [] mkRefundsData refunds,
          amountRefunded = realToFrac <$> amount_refunded,
          ..
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
              txnId = justOrder.txn_id,
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
              upi = castUpi <$> justOrder.upi,
              refunds = maybe [] mkRefundsData justOrder.refunds,
              amountRefunded = realToFrac <$> justOrder.amount_refunded
            }
        Nothing -> do
          let (isRetriedOrder, retargetPaymentLink, retargetPaymentLinkExpiry, isRetargetedOrder) = parseRetargetAndRetryData justOrder.metadata justOrder.links justOrder.additional_info
          OrderStatusResp
            { eventName = Just eventName,
              orderShortId = justOrder.order_id,
              transactionUUID = justOrder.txn_uuid,
              txnId = justOrder.txn_id,
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
              dateCreated = justOrder.date_created,
              refunds = maybe [] mkRefundsData justOrder.refunds,
              amountRefunded = realToFrac <$> justOrder.amount_refunded,
              ..
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
          responseCode = listToMaybe $ catMaybes [justNotification.response_code, justNotification.provider_response >>= (.provider_response_code)],
          responseMessage = listToMaybe $ catMaybes [justNotification.response_message, justNotification.provider_response >>= (.provider_response_message)],
          notificationId = justNotification.object_reference_id
        }
    (_, _, _, Just justTransaction) -> do
      let (isRetriedOrder, retargetPaymentLink, retargetPaymentLinkExpiry, isRetargetedOrder) = parseRetargetAndRetryData justTransaction.metadata justTransaction.links justTransaction.additional_info
      OrderStatusResp
        { eventName = Just eventName,
          orderShortId = justTransaction.order_id,
          transactionUUID = justTransaction.txn_uuid,
          txnId = Just justTransaction.txn_id,
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
          dateCreated = Nothing,
          refunds = [],
          amountRefunded = Nothing,
          ..
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
    { order = mkOfferOrder order planId registrationDate dutyDate paymentMode numOfRides offerListingMetric,
      payment_method_info = [],
      customer = mkOfferCustomer <$> customer,
      offer_code = Nothing
    }

mkOfferOrder :: OfferOrder -> Text -> UTCTime -> UTCTime -> Text -> Int -> Maybe UDF6 -> Juspay.OfferOrder
---- add duty day and payment mode respectively in holes ----
mkOfferOrder OfferOrder {..} planId registrationDate dutyDate paymentMode numOfRides offerListingMetric =
  Juspay.OfferOrder
    { order_id = orderId,
      amount = show amount,
      currency,
      udf1 = replace "-" "_" planId,
      udf2 = pack $ formatTime defaultTimeLocale "%d_%m_%y" registrationDate,
      udf3 = paymentMode,
      udf4 = pack $ formatTime defaultTimeLocale "%d_%m_%y" dutyDate,
      udf5 = do
        let strNumRides = show numOfRides
        if strNumRides == "-1" then "DEFAULT" else strNumRides,
      udf6 = parseUDF6 <$> offerListingMetric
    }
  where
    parseUDF6 offerListingMetric' = do
      case offerListingMetric' of
        LIST_BASED_ON_DATE listingDates -> pack $ formatTime defaultTimeLocale "%d_%m_%y" listingDates
        _ -> show offerListingMetric'

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
  m AutoRefundResp
autoRefund config req = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  mkRefundResp <$> Juspay.autoRefund url apiKey req.orderId (mkAutoPayRequest req)
  where
    mkAutoPayRequest request =
      Juspay.AutoRefundReq
        { unique_request_id = request.requestId,
          amount = realToFrac request.amount
        }

mkRefundResp :: Juspay.AutoRefundResp -> AutoRefundResp
mkRefundResp Juspay.AutoRefundResp {..} = do
  AutoRefundResp
    { orderId = order_id,
      merchantId = merchant_id,
      customerId = customer_id,
      currency,
      amountRefunded = amount_refunded,
      refunds = maybe [] mkRefundsData refunds
    }

mkRefundsData :: [Juspay.RefundsData] -> [RefundsData]
mkRefundsData =
  map
    ( \Juspay.RefundsData {..} ->
        RefundsData
          { idAssignedByServiceProvider = id,
            amount = realToFrac amount,
            status = status,
            errorMessage = error_message,
            errorCode = error_code,
            initiatedBy = initiated_by,
            requestId = unique_request_id
          }
    )

parseRetargetAndRetryData ::
  Maybe Juspay.MetaData ->
  Maybe Juspay.LinkData ->
  Maybe Juspay.AdditionalInfo ->
  (Maybe Bool, Maybe Text, Maybe UTCTime, Maybe Bool)
parseRetargetAndRetryData metaData linkData additionalInfo = do
  let retargetInfoFromMetaData = metaData >>= (.juspay_internal_retarget_configs)
      retargetInfoFromAdditionalInfo = additionalInfo >>= (.retarget_payment_info)
      retargetInfoFromLinkData = linkData >>= (.retarget_payment_links)
      functionsToCalls = [Juspay.retarget_payment_link, Juspay.is_retargeted_order, Juspay.retarget_payment_link_expiry, Juspay.retarget_done_count, Juspay.max_retarget_limit]
      [retargetLink, isRetargetedOrder, retargetPaymentLinkExpiry, _, _] = map (getFieldData retargetInfoFromMetaData retargetInfoFromLinkData retargetInfoFromAdditionalInfo) functionsToCalls
      retargetPaymentLinkExpiryTime = (\val -> readMaybe val :: Maybe UTCTime) . T.unpack =<< retargetPaymentLinkExpiry
      isRetriedOrder = (metaData >>= (.is_retried_order)) E.<|> (additionalInfo >>= (.mandate_retry_info) >>= (.is_retried_order))
  (getBoolValue =<< isRetriedOrder, retargetLink, retargetPaymentLinkExpiryTime, getBoolValue =<< isRetargetedOrder)
  where
    getFieldData retargetInfoFromMetaData retargetInfoFromLinkData retargetInfoFromAdditionalInfo func = do
      listToMaybe $ mapMaybe (>>= func) [retargetInfoFromMetaData, retargetInfoFromLinkData, retargetInfoFromAdditionalInfo]
    getBoolValue = (\val -> readMaybe val :: Maybe Bool) . T.unpack
