module Kernel.External.SMS.Interface.PinbixSms
  ( module Reexport,
    sendOTP,
  )
where

import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.SMS.Interface.Types as Reexport
import Kernel.External.SMS.PinbixSms.Config
import qualified Kernel.External.SMS.PinbixSms.Flow as PF
import Kernel.External.SMS.PinbixSms.Types
import Kernel.External.SMS.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Servant.Client ()

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    MonadReader r m,
    Log m
  ) =>
  PinbixSmsCfg ->
  SendSMSReq ->
  m SendSMSRes
sendOTP pinbixCfg SendSMSReq {..} = do
  let pinbixPhoneNumber =
        if not (T.null phoneNumber) && T.head phoneNumber == '+'
          then T.drop 1 phoneNumber
          else phoneNumber

  pinbixPassword <- decrypt pinbixCfg.password
  resp <- PF.sendOTPApi smsBody pinbixPhoneNumber sender pinbixCfg.userId pinbixPassword pinbixCfg
  pure $ mapResponse resp

mapResponse :: PinbixSmsResponse -> SendSMSRes
mapResponse PinbixSmsResponse {..}
  | T.toLower status == "success" = Success
  | T.toLower status == "error" = Fail
  | otherwise = UnknownError
