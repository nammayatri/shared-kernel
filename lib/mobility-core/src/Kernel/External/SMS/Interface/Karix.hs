module Kernel.External.SMS.Interface.Karix
  ( module Reexport,
    sendOTP,
  )
where

import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.SMS.Interface.Types as IT
import Kernel.External.SMS.Karix.Config
import qualified Kernel.External.SMS.Karix.Flow as KF
import Kernel.External.SMS.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m
  ) =>
  KarixCfg ->
  IT.SendSMSReq ->
  m IT.SendSMSRes
sendOTP karixCfg SendSMSReq {..} = do
  let karixOtpSmsTemplate = smsBody
  let karixPhoneNumber = T.dropWhile (== '+') phoneNumber
  let karixSender = sender
  karixKey <- decrypt karixCfg.accessKey

  res <- KF.sendOTPApi karixOtpSmsTemplate karixPhoneNumber karixSender karixKey karixCfg
  return $ returnSmsResultKarix (res.response.status.code)

returnSmsResultKarix :: Text -> IT.SendSMSRes
returnSmsResultKarix txt = case txt of
  "200" -> Success
  _ -> Fail
