module Kernel.External.SMS.Interface.PinbixSms
  ( module Reexport,
    sendOTP,
  )
where

import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.External.SMS.Interface.Types as Reexport
import Kernel.External.SMS.PinbixSms.Config
import qualified Kernel.External.SMS.PinbixSms.Flow as PF
import Kernel.External.SMS.PinbixSms.Types
import Kernel.External.SMS.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Logging (logDebug)

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m,
    Log m
  ) =>
  PinbixSmsCfg ->
  SendSMSReq ->
  m SendSMSRes
sendOTP pinbixCfg SendSMSReq {..} = do
  let pinbixSmsTemplate = "3333+is+your+OTP+for+login+to+Namma+Yatri+App.+test+-Namma+Yatri"
      pinbixPhoneNumber =
        if not (T.null phoneNumber) && T.head phoneNumber == '+'
          then T.drop 1 phoneNumber
          else phoneNumber
      pinbixSender = "NMAYTI"

  logDebug $ "[Pinbix] Initiating sendOTP for " <> pinbixPhoneNumber
  resp <- PF.sendOTPApi pinbixSmsTemplate pinbixPhoneNumber pinbixSender pinbixCfg.userId pinbixCfg.password pinbixCfg
  logDebug $ "[Pinbix] Provider responded with status " <> resp.status <> maybe "" (" code " <>) resp.statusCode
  pure $ mapResponse resp

mapResponse :: PinbixSmsResponse -> SendSMSRes
mapResponse PinbixSmsResponse {..}
  | T.toLower status == "success" = Success
  | T.toLower status == "error" = Fail
  | otherwise = UnknownError
