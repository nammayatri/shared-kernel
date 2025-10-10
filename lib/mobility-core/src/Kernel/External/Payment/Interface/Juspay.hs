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
    createCustomer,
    getCustomer,
    orderStatus,
    updateOrder,
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
    verifyVPA,
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
import qualified Kernel.External.Payment.Juspay.Types.CreateCustomer as Customer
import qualified Kernel.External.Payment.Juspay.Webhook as Juspay
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.Ack
import Kernel.Types.Error
import Kernel.Utils.Common (HighPrecMoney, Log, MonadTime, fromMaybeM, getCurrentTime)
import Kernel.Utils.Logging (logDebug)
import Servant hiding (throwError)

createOrder ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  Maybe Text ->
  CreateOrderReq ->
  m CreateOrderResp
createOrder config mRoutingId req = do
  let url = config.url
      merchantId = config.merchantId
      clientId = fromMaybe merchantId config.pseudoClientId
  apiKey <- decrypt config.apiKey
  orderReq <- mkCreateOrderReq config.returnUrl clientId merchantId req
  Juspay.createOrder url apiKey merchantId mRoutingId orderReq

updateOrder ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  Maybe Text ->
  OrderUpdateReq ->
  m OrderUpdateResp
updateOrder config mRoutingId req = do
  let url = config.url
      merchantId = config.merchantId
  logDebug $ "updateOrder req: " <> show req
  apiKey <- decrypt config.apiKey
  updateOrderReq <- mkUpdateOrderReq req
  logDebug $ "updateOrder mkUpdateOrderReq: " <> show updateOrderReq
  updateOrderRes <- Juspay.updateOrder url apiKey merchantId req.orderShortId mRoutingId updateOrderReq
  logDebug $ "updateOrder res: " <> show updateOrderRes
  return $ mkUpdateOrderRes updateOrderRes
  where
    mkUpdateOrderReq :: MonadTime m => OrderUpdateReq -> m Juspay.OrderUpdateReq
    mkUpdateOrderReq OrderUpdateReq {..} =
      do
        return
          Juspay.OrderUpdateReq
            { amount = amount,
              split_settlement_details = mkSplitSettlementDetailsAmountBased <$> req.splitSettlementDetails
            }
    mkUpdateOrderRes Juspay.OrderUpdateResp {..} =
      OrderUpdateResp
        { orderId = order_id,
          amount
        }

createCustomer ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  CreateCustomerReq ->
  m CreateCustomerResp
createCustomer config req = do
  let url = config.url
      merchantId = config.merchantId
      routingId = req.objectReferenceId
  apiKey <- decrypt config.apiKey
  createCustomerReq <- mkcreateCustomerReq req
  creatCustomerRespo <- Juspay.createCustomer url apiKey merchantId routingId createCustomerReq
  return $ mkCreateCustomerRes creatCustomerRespo
  where
    mkcreateCustomerReq :: MonadTime m => CreateCustomerReq -> m Juspay.CreateCustomerRequest
    mkcreateCustomerReq CreateCustomerReq {..} =
      do
        return
          Juspay.CreateCustomerRequest
            { object_reference_id = fromMaybe "" objectReferenceId,
              mobile_number = fromMaybe "" phone,
              email_address = email,
              first_name = name,
              last_name = lastName,
              mobile_country_code = mobileCountryCode,
              options_get_client_auth_token = optionsGetClientAuthToken
            }
    mkCreateCustomerRes Juspay.CreateCustomerResp {..} =
      CreateCustomerResp
        { customerId = object_reference_id,
          clientAuthToken = Customer.client_auth_token <$> juspay,
          clientAuthTokenExpiry = Customer.client_auth_token_expiry <$> juspay
        }

getCustomer ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  CustomerId ->
  m CreateCustomerResp
getCustomer config customerId = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  creatCustomerRespo <- Juspay.getCustomer url apiKey merchantId (Just customerId) customerId mkGetCustomerReq
  return $ mkCreateCustomerRes creatCustomerRespo
  where
    mkCreateCustomerRes Juspay.CreateCustomerResp {..} =
      CreateCustomerResp
        { customerId = object_reference_id,
          clientAuthToken = Customer.client_auth_token <$> juspay,
          clientAuthTokenExpiry = Customer.client_auth_token_expiry <$> juspay
        }
    mkGetCustomerReq =
      Juspay.GetCustomerReq
        { options_get_client_auth_token = True
        }

mandateNotification ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  Maybe Text ->
  MandateNotificationReq ->
  m MandateNotificationRes
mandateNotification config mRoutingId req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  notificationResponse <- Juspay.mandateNotification url apiKey req.mandateId merchantId mRoutingId (mkNotificationReq req)
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
  Maybe Text ->
  NotificationStatusReq ->
  m NotificationStatusResp
mandateNotificationStatus config mRoutingId req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  notificationStatusResponse <- Juspay.mandateNotificationStatus url apiKey req.notificationId merchantId mRoutingId
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
  Maybe Text ->
  MandateExecutionReq ->
  m MandateExecutionRes
mandateExecution config mRoutingId req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  executionResponse <- Juspay.mandateExecution url apiKey merchantId mRoutingId (mkExecutionReq req merchantId)
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
  Maybe Text ->
  MandateRevokeReq ->
  m MandateRevokeRes
mandateRevoke config mRoutingId req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  void $ Juspay.mandateRevoke url apiKey merchantId mRoutingId req.mandateId Juspay.MandateRevokeReq {command = "revoke"}
  return Success

mkCreateOrderReq :: MonadTime m => BaseUrl -> Text -> Text -> CreateOrderReq -> m Juspay.CreateOrderReq
mkCreateOrderReq returnUrl clientId merchantId CreateOrderReq {..} =
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
          metadata_mandate_name = if isJust createMandate then Just (toUpper merchantId) else Nothing,
          metadata_remarks = ("Amount to be paid now is Rs " <>) . show . double2Int . realToFrac $ amount,
          mandate_start_date = mandateStartDate,
          mandate_end_date = mandateEndDate,
          options_get_upi_deep_links = optionsGetUpiDeepLinks,
          metadata_expiry_in_mins = metadataExpiryInMins,
          metadata_gateway_reference_id = metadataGatewayReferenceId,
          split_settlement_details = mkSplitSettlementDetails <$> splitSettlementDetails,
          basket = show <$> basket
        }

mkSplitSettlementDetailsAmountBased :: SplitSettlementDetailsAmount -> Juspay.SplitSettlementDetailsAmount
mkSplitSettlementDetailsAmountBased splitDetails =
  Juspay.SplitSettlementDetailsAmount
    { marketplace = mkMarketplace splitDetails.marketplace,
      mdr_borne_by = show splitDetails.mdrBorneBy,
      vendor = mkVendor splitDetails.vendor
    }
  where
    mkMarketplace Marketplace {..} = Juspay.MarketplaceAmount {..}
    mkVendor vendor = Juspay.VendorAmount {split = mkSplit <$> vendor.split}
    mkSplit split = Juspay.SplitAmount {amount = split.amount, merchant_commission = split.merchantCommission, sub_mid = split.subMid, unique_split_id = Just split.uniqueSplitId}

mkSplitSettlementDetails :: SplitSettlementDetails -> Juspay.SplitSettlementDetails
mkSplitSettlementDetails = \case
  AmountBased details -> Juspay.AmountBased (mkSplitSettlementDetailsAmountBased details)
  PercentageBased details -> Juspay.PercentageBased (mkSplitSettlementDetailsPercentageBased details)

mkSplitSettlementDetailsPercentageBased :: SplitSettlementDetailsPercentage -> Juspay.SplitSettlementDetailsPercentage
mkSplitSettlementDetailsPercentageBased splitDetails =
  Juspay.SplitSettlementDetailsPercentage
    { marketplace = mkMarketplacePercentage splitDetails.marketplace,
      mdr_borne_by = show splitDetails.mdrBorneBy,
      vendor = mkVendorPercentage splitDetails.vendor
    }
  where
    mkMarketplacePercentage MarketplacePercentage {..} = Juspay.MarketplacePercentage {amount_percentage = amountPercentage}
    mkVendorPercentage vendor = Juspay.VendorPercentage {split = mkSplitPercentage <$> vendor.split}
    mkSplitPercentage split =
      Juspay.SplitPercentage
        { amount_percentage = split.amountPercentage,
          merchant_commission_percentage = fromRational (toRational split.merchantCommissionPercentage),
          sub_mid = split.subMid,
          unique_split_id = Just split.uniqueSplitId
        }

orderStatus ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  Maybe Text ->
  OrderStatusReq ->
  m OrderStatusResp
orderStatus config mRoutingId req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  mkOrderStatusResp <$> Juspay.orderStatus url apiKey merchantId mRoutingId req.orderShortId

mkOrderStatusResp :: Juspay.OrderStatusResp -> OrderStatusResp
mkOrderStatusResp Juspay.OrderData {..} =
  case mandate of
    Just justMandate ->
      MandateOrderStatusResp
        { eventName = Nothing,
          orderShortId = order_id,
          transactionUUID = txn_uuid,
          txnId = txn_id,
          transactionStatusId = fromMaybe (-3001) status_id,
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
          transactionStatusId = fromMaybe (-3001) status_id,
          transactionStatus = status,
          paymentMethodType = payment_method_type,
          paymentMethod = payment_method,
          paymentGatewayResponse =
            payment_gateway_response
              <&> ( \pgResp ->
                      PaymentGatewayResponse
                        { respCode = pgResp.resp_code,
                          rrn = pgResp.rrn,
                          created = pgResp.created,
                          epgTxnId = pgResp.epg_txn_id,
                          respMessage = pgResp.resp_message,
                          authIdCode = pgResp.auth_id_code,
                          txnId = pgResp.txn_id
                        }
                  ),
          respMessage = resp_message,
          respCode = resp_code,
          gatewayReferenceId = gateway_reference_id,
          amount = realToFrac amount,
          effectiveAmount = realToFrac <$> effective_amount,
          currency = currency,
          bankErrorMessage = if bank_error_message == Just "" then Nothing else bank_error_message,
          bankErrorCode = if bank_error_code == Just "" then Nothing else bank_error_code,
          dateCreated = date_created,
          refunds = maybe [] mkRefundsData refunds,
          amountRefunded = realToFrac <$> amount_refunded,
          payerVpa = payer_vpa,
          upi = castUpi <$> upi,
          card = castCard <$> card,
          splitSettlementResponse = mkSplitSettlementResponse <$> split_settlement_response,
          offers = maybe Nothing mkOffersData offers,
          ..
        }

castUpi :: Juspay.Upi -> Upi
castUpi Juspay.Upi {..} = Upi {payerApp = payer_app, payerAppName = payer_app_name, txnFlowType = txn_flow_type, payerVpa = payer_vpa}

castCard :: Juspay.CardInfo -> CardInfo
castCard Juspay.CardInfo {..} = CardInfo {cardType = card_type, lastFourDigits = last_four_digits}

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
      order = Juspay.MandateOrder {orderId = orderId, orderAmount = show amount, orderCustomerId = customerId, splitSettlementDetails = mkSplitSettlementDetailsAmountBased <$> splitSettlementDetails},
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
              transactionStatusId = fromMaybe (-3001) justOrder.status_id,
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
              amountRefunded = realToFrac <$> justOrder.amount_refunded -- not adding split
            }
        Nothing -> do
          let (isRetriedOrder, retargetPaymentLink, retargetPaymentLinkExpiry, isRetargetedOrder) = parseRetargetAndRetryData justOrder.metadata justOrder.links justOrder.additional_info
          OrderStatusResp
            { eventName = Just eventName,
              orderShortId = justOrder.order_id,
              transactionUUID = justOrder.txn_uuid,
              txnId = justOrder.txn_id,
              transactionStatusId = fromMaybe (-3001) justOrder.status_id,
              transactionStatus = justOrder.status,
              paymentMethodType = justOrder.payment_method_type,
              paymentMethod = justOrder.payment_method,
              paymentGatewayResponse =
                justOrder.payment_gateway_response
                  <&> ( \pgResp ->
                          PaymentGatewayResponse
                            { respCode = pgResp.resp_code,
                              rrn = pgResp.rrn,
                              created = pgResp.created,
                              epgTxnId = pgResp.epg_txn_id,
                              respMessage = pgResp.resp_message,
                              authIdCode = pgResp.auth_id_code,
                              txnId = pgResp.txn_id
                            }
                      ),
              respMessage = justOrder.resp_message,
              respCode = justOrder.resp_code,
              gatewayReferenceId = justOrder.gateway_reference_id,
              bankErrorMessage = if justOrder.bank_error_message == Just "" then Nothing else justOrder.bank_error_message,
              bankErrorCode = if justOrder.bank_error_code == Just "" then Nothing else justOrder.bank_error_code,
              amount = realToFrac justOrder.amount,
              effectiveAmount = realToFrac <$> justOrder.effective_amount,
              currency = justOrder.currency,
              dateCreated = justOrder.date_created,
              refunds = maybe [] mkRefundsData justOrder.refunds,
              amountRefunded = realToFrac <$> justOrder.amount_refunded,
              payerVpa = justOrder.payer_vpa,
              upi = castUpi <$> justOrder.upi,
              card = castCard <$> justOrder.card,
              splitSettlementResponse = mkSplitSettlementResponse <$> justOrder.split_settlement_response,
              offers = maybe Nothing mkOffersData justOrder.offers,
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
          transactionStatusId = fromMaybe (-3001) justTransaction.status_id,
          transactionStatus = justTransaction.status,
          paymentMethodType = Nothing,
          paymentMethod = Nothing,
          paymentGatewayResponse = Nothing,
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
          payerVpa = justTransaction.payer_vpa,
          upi = castUpi <$> justTransaction.upi,
          card = castCard <$> justTransaction.card,
          splitSettlementResponse = Nothing,
          effectiveAmount = Just $ realToFrac justTransaction.txn_amount,
          offers = Nothing,
          ..
        }
    (_, _, Nothing, _) -> BadStatusResp

mkSplitSettlementResponse :: Juspay.SplitSettlementResponse -> SplitSettlementResponse
mkSplitSettlementResponse Juspay.SplitSettlementResponse {..} =
  SplitSettlementResponse
    { splitDetails = split_details >>= (Just . map mkSplitDetailsResponse),
      splitApplied = split_applied
    }
  where
    mkSplitDetailsResponse Juspay.SplitDetailsResponse {..} =
      SplitDetailsResponse
        { subVendorId = sub_vendor_id,
          merchantCommission = merchant_commission,
          amount = amount,
          gatewaySubAccountId = gateway_sub_account_id,
          epgTxnId = epg_txn_id
        }

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
  Maybe Text ->
  OfferListReq ->
  m OfferListResp
offerList config mRoutingId req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  let juspayReq = mkOfferListReq req
  juspayResp <- Juspay.offerList url apiKey merchantId mRoutingId juspayReq
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
  Maybe Text ->
  OfferApplyReq ->
  m OfferApplyResp
offerApply config mRoutingId req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  let juspayReq = mkOfferApplyReq merchantId req
  juspayResp <- Juspay.offerApply url apiKey merchantId mRoutingId juspayReq
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
  Maybe Text ->
  OfferNotifyReq ->
  m OfferNotifyResp
offerNotify config mRoutingId req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  let juspayReq = mkOfferNotifyReq merchantId req
  void $ Juspay.offerNotify url apiKey merchantId mRoutingId req.mandateId juspayReq
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
  Maybe Text ->
  AutoRefundReq ->
  m AutoRefundResp
autoRefund config mRoutingId req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  mkRefundResp <$> Juspay.autoRefund url apiKey req.orderId merchantId mRoutingId (mkAutoPayRequest req)
  where
    mkAutoPayRequest request =
      Juspay.AutoRefundReq
        { unique_request_id = request.requestId,
          amount = realToFrac request.amount,
          split_settlement_details = mkSplitSettlementDetails <$> req.splitSettlementDetails
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

mkOffersData :: [Juspay.Offer] -> Maybe [Offer]
mkOffersData =
  Just
    . map
      ( \Juspay.Offer {..} ->
          Offer {offerId = offer_id, offerCode = offer_code, status = status}
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
      functionsToCalls = (Juspay.retarget_payment_link, Juspay.is_retargeted_order, Juspay.retarget_payment_link_expiry, Juspay.retarget_done_count, Juspay.max_retarget_limit)
      (retargetLink, isRetargetedOrder, retargetPaymentLinkExpiry, _, _) = mapTuple5 (getFieldData retargetInfoFromMetaData retargetInfoFromLinkData retargetInfoFromAdditionalInfo) functionsToCalls
      retargetPaymentLinkExpiryTime = (\val -> readMaybe val :: Maybe UTCTime) . T.unpack =<< retargetPaymentLinkExpiry
      isRetriedOrder = (metaData >>= (.is_retried_order)) E.<|> (additionalInfo >>= (.mandate_retry_info) >>= (.is_retried_order))
  (getBoolValue =<< isRetriedOrder, retargetLink, retargetPaymentLinkExpiryTime, getBoolValue =<< isRetargetedOrder)
  where
    getFieldData retargetInfoFromMetaData retargetInfoFromLinkData retargetInfoFromAdditionalInfo func = do
      listToMaybe $ mapMaybe (>>= func) [retargetInfoFromMetaData, retargetInfoFromLinkData, retargetInfoFromAdditionalInfo]
    getBoolValue = (\val -> readMaybe val :: Maybe Bool) . T.unpack

    mapTuple5 :: (a -> b) -> (a, a, a, a, a) -> (b, b, b, b, b)
    mapTuple5 f (a1, a2, a3, a4, a5) = (f a1, f a2, f a3, f a4, f a5)

verifyVPA ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayCfg ->
  Maybe Text ->
  VerifyVPAReq ->
  m VerifyVPAResp
verifyVPA config mRoutingId req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  mkVerifyVpaResp <$> Juspay.verifyVPA url apiKey merchantId mRoutingId (mkVerifyVpaRequest req merchantId)
  where
    mkVerifyVpaRequest request merchantId =
      Juspay.VerifyVPAReq
        { vpa = request.vpa,
          order_id = request.orderId,
          merchant_id = merchantId,
          customer_id = request.customerId
        }

mkVerifyVpaResp :: Juspay.VerifyVPAResp -> VerifyVPAResp
mkVerifyVpaResp Juspay.VerifyVPAResp {..} = do
  VerifyVPAResp
    { vpa,
      status = show status,
      customerName = customer_name
    }
