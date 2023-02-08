module Kernel.External.SMS.Interface.MyValueFirst
  ( module Reexport,
    sendOTP,
  )
where

import Kernel.External.Encryption
import Kernel.External.SMS.Interface.Types as IT
import Kernel.External.SMS.MyValueFirst.Config
import qualified Kernel.External.SMS.MyValueFirst.Flow as MVF
import Kernel.External.SMS.MyValueFirst.Types as MT
import Kernel.External.SMS.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import EulerHS.Prelude

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m
  ) =>
  MyValueFirstCfg ->
  IT.SendSMSReq ->
  m IT.SendSMSRes
sendOTP smsCfg SendSMSReq {..} = do
  let urlAddress = smsCfg.url
      otpSmsTemp = smsBody
      phone = phoneNumber
      senderName = sender
  username <- decrypt smsCfg.username
  password <- decrypt smsCfg.password
  res <- MVF.sendOTPApi urlAddress username password otpSmsTemp phone senderName

  return $ returnSmsResultMVF res

returnSmsResultMVF :: MT.SubmitSmsRes -> IT.SendSMSRes
returnSmsResultMVF txt =
  case txt of
    Sent -> Success
    BadNumber -> Fail
    _ -> IT.UnknownError
