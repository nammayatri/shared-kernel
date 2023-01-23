module Beckn.External.SMS.Interface.MyValueFirst
  ( module Reexport,
    sendOTP,
  )
where

import Beckn.External.Encryption
import Beckn.External.SMS.Interface.Types as IT
import Beckn.External.SMS.MyValueFirst.Config
import qualified Beckn.External.SMS.MyValueFirst.Flow as MVF
import Beckn.External.SMS.MyValueFirst.Types as MT
import Beckn.External.SMS.Types as Reexport
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
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
