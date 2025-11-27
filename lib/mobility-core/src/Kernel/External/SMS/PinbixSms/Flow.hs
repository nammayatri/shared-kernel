{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Kernel.External.SMS.PinbixSms.Flow where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.External.SMS.PinbixSms.Api as API
import Kernel.External.SMS.PinbixSms.Config
import Kernel.External.SMS.PinbixSms.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common as B
import Network.HTTP.Types.URI (renderQuery)
import Servant.Client

sendOTPApi ::
  ( CoreMetrics m,
    MonadFlow m,
    B.Log m
  ) =>
  -- | SMS text
  Text ->
  -- | Phone number
  Text ->
  -- | Sender ID
  Text ->
  -- | User id
  Text ->
  -- | Password
  Text ->
  PinbixSmsCfg ->
  m PinbixSmsResponse
sendOTPApi otpSmsTemplate phoneNumber pinbixSender pinbixUserId pinbixPassword PinbixSmsCfg {..} = do
  logDebug $ "[Pinbix] Preparing request for " <> phoneNumber
  let encodedMsg = T.replace " " "+" otpSmsTemplate
      eulerClient = ET.client API.pinbixSmsSendAPI
  logDebug $ "[Pinbix] Encoded message: " <> encodedMsg
  let queryParams =
        [ ("userid", pinbixUserId),
          ("password", pinbixPassword),
          ("sendMethod", sendMethod),
          ("senderid", pinbixSender),
          ("msgType", msgType),
          ("output", output),
          ("mobile", phoneNumber),
          ("msg", encodedMsg)
        ]
      renderPair (k, v) = (TE.encodeUtf8 k, Just (TE.encodeUtf8 v))
      queryString =
        T.pack $
          BS.unpack $
            renderQuery True (fmap renderPair queryParams)
      finalUrl = T.pack (showBaseUrl url) <> queryString
  logDebug $ "[Pinbix] Full URL: " <> finalUrl

  rawRes <-
    callAPI
      url
      ( eulerClient
          pinbixUserId
          pinbixPassword
          sendMethod
          pinbixSender
          msgType
          output
          phoneNumber
          encodedMsg
      )
      "sendOTPApi"
      API.pinbixSmsSendAPI
  logDebug "[Pinbix] Call completed, parsing response"
  fromEitherM (pinbixSmsError url) rawRes >>= validatePinbixResponse

pinbixSmsError :: BaseUrl -> ClientError -> ExternalAPICallError
pinbixSmsError url err = ExternalAPICallError (Just "PINBIX_SMS_API_ERROR") url err

validatePinbixResponse ::
  (MonadThrow m, B.Log m) =>
  PinbixSmsResponse ->
  m PinbixSmsResponse
validatePinbixResponse resp@PinbixSmsResponse {..}
  | T.toLower status == "success" = do
    logDebug "[Pinbix] Response status=success"
    pure resp
  | otherwise = do
    logDebug $
      "[Pinbix] Response indicates failure. status="
        <> status
        <> maybe "" (" code " <>) statusCode
        <> maybe "" (" reason: " <>) reason
    throwError $
      PinbixSmsError
        { pinbixStatus = status,
          pinbixStatusCode = statusCode,
          pinbixReason = reason
        }
