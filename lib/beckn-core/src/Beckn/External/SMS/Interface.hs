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
import qualified Data.Text as T
import EulerHS.Prelude

constructSendSMSReq :: Text -> Text -> Text -> Text -> Text -> SendSMSReq
constructSendSMSReq otpCode otpHash otpSmsTemplate phone sender =
  SendSMSReq
    { smsBody = constructOtpSms otpCode otpHash otpSmsTemplate,
      phoneNumber = phone,
      sender = sender
    }

sendSMS :: (EncFlow m r, EsqDBFlow m r, CoreMetrics m) => SmsHandler m -> SendSMSReq -> m SendSMSRes
sendSMS SmsHandler {..} req = do
  prividersPriorityList <- getProvidersPriorityList
  when (null prividersPriorityList) $ throwError $ InternalError "No sms serive provider configured"
  sendSmsWithFallback prividersPriorityList
  where
    sendSmsWithFallback [] = throwError $ InternalError "Not able to send sms with all the configured providers"
    sendSmsWithFallback (preferredProvider : restProviders) = do
      smsConfig <- getProviderConfig preferredProvider
      result <- try @_ @SomeException $ sendSMS' smsConfig req
      case result of
        Left _ -> sendSmsWithFallback restProviders
        Right res -> case res of
          UnknownError -> sendSmsWithFallback restProviders
          Fail -> sendSmsWithFallback restProviders
          _ -> pure res

sendSMS' ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  SmsServiceConfig ->
  SendSMSReq ->
  m SendSMSRes
sendSMS' serviceConfig req = case serviceConfig of
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
