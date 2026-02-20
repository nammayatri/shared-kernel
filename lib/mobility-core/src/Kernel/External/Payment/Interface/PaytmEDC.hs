{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Payment.Interface.PaytmEDC
  ( module Kernel.External.Payment.Interface.PaytmEDC,
    module Reexport,
  )
where

import qualified Data.Map.Strict as Map
import Kernel.External.Encryption
import Kernel.External.Payment.Interface.Types
import Kernel.External.Payment.Juspay.Types.CreateOrder (SDKPayload (..), SDKPayloadDetails (..))
import Kernel.External.Payment.PaytmEDC.Checksum
import Kernel.External.Payment.PaytmEDC.Config as Reexport
import qualified Kernel.External.Payment.PaytmEDC.Flow as PaytmEDC
import qualified Kernel.External.Payment.PaytmEDC.Types as PaytmEDC
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common

-- Create order -> PaytmEDC Sale API
-- Terminal ID still we don't how to get this.
createOrder ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m,
    MonadThrow m
  ) =>
  PaytmEDCCfg ->
  Maybe Text ->
  CreateOrderReq ->
  m CreateOrderResp
createOrder cfg _mRoutingId req = do
  now <- getCurrentTime

  -- Decrypt config
  let paytmMid = cfg.paytmMid
      channelId = cfg.channelId
      clientId = cfg.clientId

  logDebug $ "createOrder req: " <> show req
  logDebug $ "createOrder cfg: " <> show cfg
  logDebug $ "createOrder paytmMid: " <> show paytmMid
  logDebug $ "createOrder channelId: " <> show channelId
  logDebug $ "createOrder clientId: " <> show clientId

  let terminalId = req.metadataGatewayReferenceId -- TID passed via metadata
      amountInPaise = round (req.amount * 100) :: Int
      timestamp = PaytmEDC.formatPaytmTimestamp now

      -- Checksum params — everything from the sale body including callbackUrl
      checksumParams =
        Map.fromList $
          [ ("paytmMid", paytmMid),
            ("transactionDateTime", timestamp),
            ("merchantTransactionId", PaytmEDC.removeHyphens req.orderId),
            ("merchantReferenceNo", PaytmEDC.removeHyphens req.orderShortId),
            ("transactionAmount", show amountInPaise)
          ]
            <> maybe [] (\tid -> [("paytmTid", tid)]) terminalId
      -- Checksum Request
      checksumReqBody =
        ChecksumRequestBody
          { params = checksumParams,
            mapParams = Nothing
          }

      checksumReqHead =
        ChecksumRequestHead
          { mid = paytmMid,
            tid = terminalId,
            clientId = clientId
          }

      checksumReq =
        GenerateChecksumReq
          { gcReqHead = checksumReqHead,
            gcReqBody = checksumReqBody
          }

  -- Call Checksum API
  logDebug $ "createOrder checksumReq : " <> show (toJSON checksumReq)
  checksumResp <- PaytmEDC.generateChecksum cfg.baseUrl checksumReq
  logDebug $ "createOrder checksumResp : " <> show (toJSON checksumResp)
  let ri = checksumResp.gcRespBody.resultInfo
  checksum <-
    fromMaybeM
      ( InternalError $
          "PaytmEDC checksum failed - resultStatus: " <> PaytmEDC.resultStatus ri
            <> ", resultCode: "
            <> PaytmEDC.resultCode ri
            <> ", resultMsg: "
            <> PaytmEDC.resultMsg ri
            <> maybe "" (", resultCodeId: " <>) (PaytmEDC.resultCodeId ri)
      )
      (checksumResp.gcRespBody.checksum)

  -- Build Sale Request with checksum
  let saleBody =
        PaytmEDC.PaytmEDCSaleRequestBody
          { paytmMid = paytmMid,
            paytmTid = terminalId,
            transactionDateTime = timestamp,
            merchantTransactionId = req.orderId,
            merchantReferenceNo = Just req.orderShortId,
            transactionAmount = show amountInPaise
          }

      saleHead =
        PaytmEDC.PaytmEDCRequestHead
          { requestTimeStamp = timestamp,
            channelId = channelId,
            checksum = checksum
          }

      saleRequest =
        PaytmEDC.PaytmEDCSaleRequest
          { saleRequestHead = saleHead,
            saleRequestBody = saleBody
          }
  logDebug $ "createOrder saleRequest: " <> show (toJSON saleRequest)
  response <- PaytmEDC.initiateSale cfg.baseUrl saleRequest
  logDebug $ "createOrder response: " <> show (toJSON response)
  pure $ mkCreateOrderResp response req now

-- Order status - maps to PaytmEDC Status Enquiry API
orderStatus ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m,
    MonadThrow m
  ) =>
  PaytmEDCCfg ->
  Maybe Text ->
  OrderStatusReq ->
  m OrderStatusResp
orderStatus cfg _mRoutingId orderStatusReq = do
  now <- getCurrentTime

  -- Decrypt config
  let paytmMid = cfg.paytmMid
      channelId = cfg.channelId
      clientId = cfg.clientId

  logDebug $ "orderStatus cfg: " <> show cfg
  logDebug $ "orderStatus paytmMid: " <> show paytmMid
  logDebug $ "orderStatus channelId: " <> show channelId
  logDebug $ "orderStatus clientId: " <> show clientId
  paytmTid <- fromMaybeM (InternalError "Terminal ID is required") (orderStatusReq.terminalId)
  let transactionDateTime = PaytmEDC.formatPaytmTimestamp $ fromMaybe now orderStatusReq.transactionDateTime
  -- Base params for checksum
  let baseParams =
        Map.fromList
          [ ("paytmMid", paytmMid),
            ("merchantTransactionId", PaytmEDC.removeHyphens orderStatusReq.orderId),
            ("transactionDateTime", transactionDateTime),
            ("paytmTid", paytmTid)
          ]

      -- Checksum Request
      checksumReqBody =
        ChecksumRequestBody
          { params = baseParams,
            mapParams = Nothing
          }

      checksumReqHead =
        ChecksumRequestHead
          { mid = paytmMid,
            tid = Just paytmTid,
            clientId = clientId
          }

      checksumReq =
        GenerateChecksumReq
          { gcReqHead = checksumReqHead,
            gcReqBody = checksumReqBody
          }

  -- Call Checksum API
  logDebug $ "orderStatus checksumReq: " <> show (toJSON checksumReq)
  checksumResp <- PaytmEDC.generateChecksum cfg.baseUrl checksumReq
  logDebug $ "orderStatus checksumResp: " <> show (toJSON checksumResp)
  let ri = checksumResp.gcRespBody.resultInfo
  checksum <-
    fromMaybeM
      ( InternalError $
          "PaytmEDC checksum failed - resultStatus: " <> PaytmEDC.resultStatus ri
            <> ", resultCode: "
            <> PaytmEDC.resultCode ri
            <> ", resultMsg: "
            <> PaytmEDC.resultMsg ri
            <> maybe "" (", resultCodeId: " <>) (PaytmEDC.resultCodeId ri)
      )
      (checksumResp.gcRespBody.checksum)

  -- Build Status Request with checksum
  let statusBody =
        PaytmEDC.PaytmEDCStatusRequestBody
          { paytmMid = paytmMid,
            paytmTid = Just paytmTid,
            merchantTransactionId = PaytmEDC.removeHyphens orderStatusReq.orderId,
            transactionDateTime = transactionDateTime
          }

      statusHead =
        PaytmEDC.PaytmEDCRequestHead
          { requestTimeStamp = PaytmEDC.formatPaytmTimestamp now,
            channelId = channelId,
            checksum = checksum
          }

      statusRequest =
        PaytmEDC.PaytmEDCStatusRequest
          { statusRequestHead = statusHead,
            statusRequestBody = statusBody
          }
  logDebug $ "orderStatus statusRequest: " <> show statusRequest
  response <- PaytmEDC.statusEnquiry cfg.baseUrl statusRequest
  logDebug $ "orderStatus response: " <> show response
  pure $ mkOrderStatusResp response orderStatusReq.orderId

-- Map PaytmEDC response to CreateOrderResp
-- Note: For sale API, ACCEPTED_SUCCESS maps to NEW (not CHARGED)
-- CHARGED status is only set when status API returns SUCCESS/ACCEPTED_SUCCESS
mkCreateOrderResp :: PaytmEDC.PaytmEDCResponse -> CreateOrderReq -> UTCTime -> CreateOrderResp
mkCreateOrderResp response req now =
  let respBody = PaytmEDC.responseBody response
      resultCode = PaytmEDC.parseResultCodeId respBody.resultInfo.resultCodeId
      txnStatus = case PaytmEDC.resultCodeToStatus resultCode of
        PaytmEDC.EDC_ACCEPTED_SUCCESS -> NEW -- Sale API: ACCEPTED_SUCCESS means transaction initiated, not completed
        PaytmEDC.EDC_FAILED -> AUTHENTICATION_FAILED
        PaytmEDC.EDC_PENDING -> PENDING_VBV
   in CreateOrderResp
        { id = fromMaybe req.orderId respBody.merchantTransactionId,
          order_id = req.orderShortId,
          status = txnStatus,
          sdk_payload = defaultSDKPayload req now,
          sdk_payload_json = Nothing,
          payment_links = Nothing
        }

-- createOrder -> CreateOrderResp -> SDKPayload
-- for now we are not using this.  it's juspay one we can do something about it later.
-- Create a minimal SDKPayload for EDC (not actually used for EDC payments)
-- do we really need this? we can simply return empty payload and put like maybe type in create order response
-- but still if we remove then we have to change to much of logic. so for now we are keeping this.
defaultSDKPayload :: CreateOrderReq -> UTCTime -> SDKPayload
defaultSDKPayload req now =
  SDKPayload
    { requestId = Nothing,
      service = Nothing,
      payload =
        SDKPayloadDetails
          { clientId = Nothing,
            amount = show req.amount,
            merchantId = Nothing,
            clientAuthToken = "default token",
            clientAuthTokenExpiry = now, -- Use current time as placeholder
            environment = Nothing,
            options_getUpiDeepLinks = Nothing,
            lastName = req.customerLastName,
            action = Nothing,
            customerId = Just req.customerId,
            returnUrl = Nothing,
            currency = INR, -- Default to INR
            firstName = req.customerFirstName,
            customerPhone = Just req.customerPhone,
            customerEmail = Just req.customerEmail,
            orderId = Just req.orderId,
            description = Nothing,
            createMandate = req.createMandate,
            mandateMaxAmount = show <$> req.mandateMaxAmount,
            mandateStartDate = req.mandateStartDate,
            mandateEndDate = req.mandateEndDate,
            basket = Nothing
          }
    }

-- Map PaytmEDC response to OrderStatusResp
mkOrderStatusResp :: PaytmEDC.PaytmEDCResponse -> Text -> OrderStatusResp
mkOrderStatusResp response orderId =
  let respBody = PaytmEDC.responseBody response
      resultCode = PaytmEDC.parseResultCodeId respBody.resultInfo.resultCodeId
      paytmStatus = PaytmEDC.resultCodeToStatus resultCode
      txnStatus = case paytmStatus of
        PaytmEDC.EDC_ACCEPTED_SUCCESS -> CHARGED
        PaytmEDC.EDC_FAILED -> AUTHORIZATION_FAILED
        PaytmEDC.EDC_PENDING -> PENDING_VBV
      paymentStatus = case paytmStatus of
        PaytmEDC.EDC_ACCEPTED_SUCCESS -> Just TXN_CHARGED
        PaytmEDC.EDC_FAILED -> Just TXN_FAILED
        PaytmEDC.EDC_PENDING -> Nothing
      txnAmount = maybe 0 (fromMaybe 0 . readMaybe . toString) respBody.transactionAmount
      -- Use detailed error message from result code
      errorMessage = PaytmEDC.getResultCodeMessage resultCode
   in OrderStatusResp
        { eventName = paymentStatus,
          orderShortId = orderId,
          transactionUUID = respBody.txnId,
          txnId = respBody.bankTxnId,
          transactionStatusId = 0,
          transactionStatus = txnStatus,
          paymentMethodType = respBody.paymentMode,
          paymentMethod = respBody.paymentMode,
          paymentGatewayResponse = Nothing,
          respMessage = Just errorMessage, -- Use our detailed error message
          respCode = respBody.resultInfo.resultCodeId, -- Pass the actual result code ID
          gatewayReferenceId = respBody.txnId,
          bankErrorMessage = if PaytmEDC.isSuccessCode resultCode then Nothing else Just respBody.resultInfo.resultMsg,
          bankErrorCode = if PaytmEDC.isSuccessCode resultCode then Nothing else respBody.resultInfo.resultCodeId,
          amount = txnAmount / 100, -- Convert paise to rupees
          currency = INR,
          dateCreated = Nothing,
          isRetriedOrder = Nothing,
          isRetargetedOrder = Nothing,
          retargetPaymentLink = Nothing,
          retargetPaymentLinkExpiry = Nothing,
          amountRefunded = Nothing,
          refunds = [],
          payerVpa = Nothing,
          upi = Nothing,
          card = Nothing,
          splitSettlementResponse = Nothing,
          effectiveAmount = Nothing,
          offers = Nothing
        }
