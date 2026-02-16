module Kernel.External.SMS.Interface.KarixSms
  ( module Reexport,
    sendOTP,
  )
where

import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.SMS.Interface.Types as IT
import Kernel.External.SMS.KarixSms.Config
import qualified Kernel.External.SMS.KarixSms.Flow as KF
import Kernel.External.SMS.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Servant.Client

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m,
    HasRequestId r,
    MonadReader r m
  ) =>
  KarixSmsCfg ->
  IT.SendSMSReq ->
  m IT.SendSMSRes
sendOTP karixSmsCfg SendSMSReq {..} = do
  let karixSmsOtpSmsTemplate = smsBody
  let karixSmsPhoneNumber = if not (T.null phoneNumber) && T.head phoneNumber == '+' then T.drop 1 phoneNumber else phoneNumber
  let karixSmsSender = sender

  karixSmsKey <- decrypt karixSmsCfg.accessKey

  res <- KF.sendOTPApi karixSmsOtpSmsTemplate karixSmsPhoneNumber karixSmsSender karixSmsKey karixSmsCfg
  return $ returnSmsResultKarixSms res.response.status.code

returnSmsResultKarixSms :: Text -> IT.SendSMSRes
returnSmsResultKarixSms txt = case txt of
  "200" -> Success
  _ -> Fail
