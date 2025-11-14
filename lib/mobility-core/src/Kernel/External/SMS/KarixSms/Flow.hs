{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Kernel.External.SMS.KarixSms.Flow where

import qualified Data.Aeson as A
import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.External.SMS.KarixSms.Api as API
import Kernel.External.SMS.KarixSms.Config
import Kernel.External.SMS.KarixSms.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common as B
import Servant.Client

-- | Send OTP via KarixSms JSON API
sendOTPApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    B.Log m
  ) =>
  Text -> --  SMS text
  Text -> --  Phone number
  Text -> --  Sender
  Text -> --  Decrypted API key
  KarixSmsCfg ->
  m KarixSmsSubmitRes
sendOTPApi otpSmsTemplate phoneNumber karixSmsSender karixSmsKey KarixSmsCfg {..} = do
  -- at first template then sender
  let msg =
        KarixSmsMessage
          { dest = [phoneNumber],
            text = otpSmsTemplate,
            send = karixSmsSender
          }
  let req =
        KarixSmsRequest
          { ver = "1.0",
            key = karixSmsKey,
            messages = [msg]
          }
  let eulerClient = ET.client API.karixSmsConnectAPI
  rawRes <- callAPI url (eulerClient req) "sendOTPApi" API.karixSmsConnectAPI
  checkkarixSmsOtpError url rawRes

checkkarixSmsOtpError :: (MonadThrow m, B.Log m) => BaseUrl -> Either ClientError KarixSmsSubmitRes -> m KarixSmsSubmitRes
checkkarixSmsOtpError url res = do
  case res of
    Left (UnsupportedContentType _ response) -> do
      case A.eitherDecode (responseBody response) of
        Left _ -> do
          fromEitherM (karixSmsOtpError url) res >>= validateKarixSmsResponse
        Right submitRes -> do
          validateKarixSmsResponse submitRes
    Left _ -> do
      fromEitherM (karixSmsOtpError url) res >>= validateKarixSmsResponse
    Right submitRes -> do
      validateKarixSmsResponse submitRes

validateKarixSmsResponse :: (MonadThrow m, B.Log m) => KarixSmsSubmitRes -> m KarixSmsSubmitRes
validateKarixSmsResponse submitRes = do
  let statusCode = submitRes.response.status.code
  case statusCode of
    "200" -> do
      pure submitRes
    "-999" -> do
      throwError KarixSmsInternalError
    "-101" -> do
      throwError KarixSmsApiVersionInvalid
    "-102" -> do
      throwError KarixSmsInvalidJSON
    "-105" -> do
      throwError KarixSmsInvalidIP
    "-106" -> do
      throwError KarixSmsAccountExpired
    "-107" -> do
      throwError KarixSmsAccountInactive
    "-108" -> do
      throwError KarixSmsAccountInvalidCredentials
    "-109" -> do
      throwError KarixSmsEncryptionOptionInvalid
    "-110" -> do
      throwError KarixSmsScheduleOptionDisable
    "-111" -> do
      throwError KarixSmsScheduleInvalidTime
    "-112" -> do
      throwError KarixSmsScheduleTimeBeyondBound
    "-113" -> do
      throwError KarixSmsDestinationEmpty
    "-114" -> do
      throwError KarixSmsDestinationInvalid
    "-115" -> do
      throwError KarixSmsMessageEmpty
    "-116" -> do
      throwError KarixSmsInvalidMsgType
    "-117" -> do
      throwError KarixSmsPortInvalid
    "-118" -> do
      throwError KarixSmsDLRTypeInvalid
    "-119" -> do
      throwError KarixSmsExpiryMinutesInvalid
    "-120" -> do
      throwError KarixSmsExpiryMinutesBeyondBound
    "-121" -> do
      throwError KarixSmsCountryCodeInvalidAppend
    "-122" -> do
      throwError KarixSmsUrlTrackInvalidOption
    "-123" -> do
      throwError KarixSmsCustReferenceIdInvalid
    "-124" -> do
      throwError KarixSmsCustReferenceIdInvalidLength
    "-125" -> do
      throwError KarixSmsTraiBlockoutTime
    "-126" -> do
      throwError KarixSmsScheduleTraiBlockoutTime
    "-127" -> do
      throwError KarixSmsDcsInvalid
    "-128" -> do
      throwError KarixSmsUdhiInvalid
    "-129" -> do
      throwError KarixSmsSenderIdEmpty
    "-130" -> do
      throwError KarixSmsInvalidSenderId
    "-131" -> do
      throwError KarixSmsInvalidTemplateId
    "-132" -> do
      throwError KarixSmsTemplateValuesEmpty
    "-142" -> do
      throwError KarixSmsAccessViolation
    "-143" -> do
      throwError KarixSmsEmptyReportingKey
    "-144" -> do
      throwError KarixSmsInvalidBatchNumber
    _ -> do
      throwError KarixSmsServerError

karixSmsOtpError :: BaseUrl -> ClientError -> ExternalAPICallError
karixSmsOtpError url err = ExternalAPICallError (Just "KARIX_SMS_JSON_API_ERROR") url err
