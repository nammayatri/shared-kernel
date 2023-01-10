module Beckn.External.SMS.Interface
  ( module Reexport,
    sendSMS,
    checkSmsResult,
    constructSendSMSReq,
  )
where

import qualified Beckn.External.SMS.Interface.ExotelSms as ExotelSms
import qualified Beckn.External.SMS.Interface.MyValueFirst as MyValueFirst
import Beckn.External.SMS.Interface.Types as Reexport
import Beckn.External.SMS.Types as Reexport
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified Data.Text as T

constructSendSMSReq :: Text -> Text -> Text -> Text -> Text -> SendSMSReq
constructSendSMSReq otpCode otpHash otpSmsTemplate phone sender = SendSMSReq
  { 
    smsBody = constructOtpSms otpCode otpHash otpSmsTemplate,
    phoneNumber = phone,
    sender = sender
  }

sendSMS ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  SmsServiceConfig ->
  SendSMSReq ->
  m SendSMSRes
sendSMS serviceConfig req = case serviceConfig of
  ExotelSmsConfig cfg -> ExotelSms.sendOTP cfg req
  MyValueFirstConfig cfg -> MyValueFirst.sendOTP cfg req

checkSmsResult ::
  (Log m, MonadThrow m) => SendSMSRes -> m ()
checkSmsResult txt =
  case txt of
    Success -> pure ()
    Fail -> throwError SMSInvalidNumber
    Pending -> pure ()
    _ -> throwError SMSInvalidNumber

constructOtpSms :: Text -> Text -> OtpTemplate -> Text
constructOtpSms otp hash =
  let otpTemp = "{#otp#}"
      hashTemp = "{#hash#}"
   in T.replace otpTemp otp . T.replace hashTemp hash