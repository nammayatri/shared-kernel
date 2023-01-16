module Beckn.External.SMS.Interface.ExotelSms
  ( module Reexport,
    sendOTP,
  )
where

import Beckn.External.Encryption
import Beckn.External.SMS.ExotelSms.Config
import qualified Beckn.External.SMS.ExotelSms.Flow as Ex
import Beckn.External.SMS.ExotelSms.Types
import Beckn.External.SMS.Interface.Types as IT
import Beckn.External.SMS.Types as Reexport
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import qualified Data.Text.Encoding as DT
import EulerHS.Prelude
import Servant

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m
  ) =>
  ExotelSmsConfig ->
  IT.SendSMSReq ->
  m IT.SendSMSRes
sendOTP exoCfg SendSMSReq {..} = do
  let sid = exoCfg.sid
  apiKey <- decrypt exoCfg.apiKey
  apiToken <- decrypt exoCfg.apiToken
  let exoUrl = exoCfg.url
      exoOtpSmsTemplate = smsBody
      exoPhoneNumber = phoneNumber
      senderName = sender
      authData =
        BasicAuthData
          (DT.encodeUtf8 apiKey)
          (DT.encodeUtf8 apiToken)
  res <- Ex.sendOTPApi exoUrl authData sid exoOtpSmsTemplate exoPhoneNumber senderName
  return $ returnSmsResultExo res.exoSMSMessage.exoStatus

returnSmsResultExo :: ExotelSmsStatus -> IT.SendSMSRes
returnSmsResultExo txt =
  case txt of
    SENT -> Success
    FAILED -> Fail
    FAILED_DND -> Fail
    QUEUED -> Pending
    SENDING -> Pending
    _ -> UnknownError
