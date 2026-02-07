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
import Kernel.Utils.Common

-- Create order -> PaytmEDC Sale API
-- Terminal ID still we don't how to get this.
createOrder ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaytmEDCCfg ->
  Maybe Text ->
  CreateOrderReq ->
  m CreateOrderResp
createOrder cfg _mRoutingId req = do
  now <- getCurrentTime

  -- Decrypt config
  paytmMidDecrypted <- decrypt cfg.paytmMid
  channelIdDecrypted <- decrypt cfg.channelId
  clientIdDecrypted <- decrypt cfg.clientId

  let terminalId = req.metadataGatewayReferenceId -- TID passed via metadata
      amountInPaise = round (req.amount * 100) :: Int
      timestamp = PaytmEDC.formatPaytmTimestamp now

      -- Base params for checksum
      baseParams =
        Map.fromList
          [ ("paytmMid", paytmMidDecrypted),
            ("paytmTid", fromMaybe "" terminalId),
            ("transactionDateTime", timestamp),
            ("merchantTransactionId", req.orderShortId),
            ("merchantReferenceNo", req.orderId),
            ("transactionAmount", show amountInPaise)
          ]

      -- Add callback URL if present
      paramsWithCallback =
        let cb = showBaseUrl cfg.callbackUrl
         in Map.insert "callbackUrl" cb baseParams

      -- Extended info map
      extendedInfoMap =
        Map.fromList
          [ ("autoAccept", "True"),
            ("paymentMode", "ALL")
          ]

      -- Checksum Request
      checksumReqBody =
        ChecksumRequestBody
          { params = paramsWithCallback,
            mapParams = Just $ Map.fromList [("merchantExtendedInfo", extendedInfoMap)]
          }

      checksumReqHead =
        ChecksumRequestHead
          { mid = paytmMidDecrypted,
            tid = terminalId,
            clientId = clientIdDecrypted
          }

      checksumReq =
        GenerateChecksumReq
          { gcReqHead = checksumReqHead,
            gcReqBody = checksumReqBody
          }

  -- Call Checksum API
  checksumResp <- PaytmEDC.generateChecksum cfg.baseUrl checksumReq
  let checksum = fromMaybe "" (checksumResp.gcRespBody.checksum)

  -- Build Sale Request with checksum
  let saleBody =
        PaytmEDC.PaytmEDCSaleRequestBody
          { paytmMid = paytmMidDecrypted,
            paytmTid = terminalId,
            transactionDateTime = timestamp,
            merchantTransactionId = req.orderShortId,
            merchantReferenceNo = Just req.orderId,
            transactionAmount = show amountInPaise,
            merchantExtendedInfo =
              Just
                PaytmEDC.MerchantExtendedInfo
                  { autoAccept = Just "True",
                    paymentMode = Just "ALL"
                  },
            callbackUrl = Just $ showBaseUrl cfg.callbackUrl
          }

      saleHead =
        PaytmEDC.PaytmEDCRequestHead
          { requestTimeStamp = timestamp,
            channelId = channelIdDecrypted,
            checksum = checksum,
            version = Just "1.0"
          }

      saleRequest =
        PaytmEDC.PaytmEDCSaleRequest
          { saleRequestHead = saleHead,
            saleRequestBody = saleBody
          }

  response <- PaytmEDC.initiateSale cfg.baseUrl saleRequest
  pure $ mkCreateOrderResp response req now

-- Order status - maps to PaytmEDC Status Enquiry API
orderStatus ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaytmEDCCfg ->
  Maybe Text ->
  Text -> -- orderShortId
  m OrderStatusResp
orderStatus cfg _mRoutingId orderShortId = do
  now <- getCurrentTime

  -- Decrypt config
  paytmMidDecrypted <- decrypt cfg.paytmMid
  channelIdDecrypted <- decrypt cfg.channelId
  clientIdDecrypted <- decrypt cfg.clientId

  let timestamp = PaytmEDC.formatPaytmTimestamp now

      -- Base params for checksum
      baseParams =
        Map.fromList
          [ ("paytmMid", paytmMidDecrypted),
            ("merchantTransactionId", orderShortId),
            ("transactionDateTime", timestamp)
          ]

      -- Checksum Request
      checksumReqBody =
        ChecksumRequestBody
          { params = baseParams,
            mapParams = Nothing
          }

      checksumReqHead =
        ChecksumRequestHead
          { mid = paytmMidDecrypted,
            tid = Nothing,
            clientId = clientIdDecrypted
          }

      checksumReq =
        GenerateChecksumReq
          { gcReqHead = checksumReqHead,
            gcReqBody = checksumReqBody
          }

  -- Call Checksum API
  checksumResp <- PaytmEDC.generateChecksum cfg.baseUrl checksumReq
  let checksum = fromMaybe "" (checksumResp.gcRespBody.checksum)

  -- Build Status Request with checksum
  let statusBody =
        PaytmEDC.PaytmEDCStatusRequestBody
          { paytmMid = paytmMidDecrypted,
            paytmTid = Nothing,
            merchantTransactionId = orderShortId,
            transactionDateTime = timestamp
          }

      statusHead =
        PaytmEDC.PaytmEDCRequestHead
          { requestTimeStamp = timestamp,
            channelId = channelIdDecrypted,
            checksum = checksum,
            version = Just "1.0"
          }

      statusRequest =
        PaytmEDC.PaytmEDCStatusRequest
          { statusRequestHead = statusHead,
            statusRequestBody = statusBody
          }

  response <- PaytmEDC.statusEnquiry cfg.baseUrl statusRequest
  pure $ mkOrderStatusResp response orderShortId

-- Map PaytmEDC response to CreateOrderResp
mkCreateOrderResp :: PaytmEDC.PaytmEDCResponse -> CreateOrderReq -> UTCTime -> CreateOrderResp
mkCreateOrderResp response req now =
  let respBody = PaytmEDC.responseBody response
      resultCode = PaytmEDC.parseResultCodeId respBody.resultInfo.resultCodeId
      txnStatus = case PaytmEDC.resultCodeToStatus resultCode of
        PaytmEDC.EDC_ACCEPTED_SUCCESS -> CHARGED
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
            clientAuthToken = "",
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
