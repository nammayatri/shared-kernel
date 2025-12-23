{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Kernel.External.SMS.PinbixSms.Flow where

import qualified Data.Text as T
import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.External.SMS.PinbixSms.Api as API
import Kernel.External.SMS.PinbixSms.Config
import Kernel.External.SMS.PinbixSms.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common as B
import Servant.Client

sendOTPApi ::
  ( CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m,
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
  let encodedMsg = T.replace " " "+" otpSmsTemplate
      eulerClient = ET.client API.pinbixSmsSendAPI
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
  fromEitherM (pinbixSmsError url) rawRes >>= validatePinbixResponse

pinbixSmsError :: BaseUrl -> ClientError -> ExternalAPICallError
pinbixSmsError url err = ExternalAPICallError (Just "PINBIX_SMS_API_ERROR") url err

validatePinbixResponse ::
  (MonadThrow m, B.Log m) =>
  PinbixSmsResponse ->
  m PinbixSmsResponse
validatePinbixResponse resp@PinbixSmsResponse {..}
  | T.toLower status == "success" = pure resp
  | otherwise =
    throwError $
      PinbixSmsError
        { pinbixStatus = status,
          pinbixStatusCode = statusCode,
          pinbixReason = reason
        }
