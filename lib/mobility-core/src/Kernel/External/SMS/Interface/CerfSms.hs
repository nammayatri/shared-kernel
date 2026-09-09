module Kernel.External.SMS.Interface.CerfSms
  ( module Reexport,
    sendOTP,
  )
where

import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.SMS.CerfSms.Config
import qualified Kernel.External.SMS.CerfSms.Flow as CF
import Kernel.External.SMS.CerfSms.Types
import Kernel.External.SMS.Interface.Types as Reexport
import Kernel.External.SMS.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Servant.Client

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m,
    Log m
  ) =>
  CerfSmsCfg ->
  SendSMSReq ->
  m SendSMSRes
sendOTP cerfCfg SendSMSReq {..} = do
  let cerfPhoneNumber =
        if not (T.null phoneNumber) && T.head phoneNumber == '+'
          then T.drop 1 phoneNumber
          else phoneNumber
      cerfMessageType = fromMaybe cerfCfg.defaultMessageType messageType
      mbTemplateId = if T.null templateId then Nothing else Just templateId
  cerfApiKey <- decrypt cerfCfg.apiKey
  resp <- case cerfCfg.transport of
    CerfHttpPush -> CF.sendPushSms smsBody cerfPhoneNumber sender cerfApiKey cerfMessageType cerfCfg
    CerfJsonPush -> CF.sendJsonSms smsBody cerfPhoneNumber sender cerfApiKey cerfMessageType mbTemplateId cerfCfg
  pure $ mapResponse resp

mapResponse :: CerfSmsResponse -> SendSMSRes
mapResponse CerfSmsResponse {..}
  | code == CF.cerfSuccessCode = Success
  | otherwise = Fail
