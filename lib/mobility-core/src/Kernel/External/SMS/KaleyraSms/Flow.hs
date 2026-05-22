module Kernel.External.SMS.KaleyraSms.Flow where

import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.External.Encryption (decrypt)
import Kernel.External.SMS.KaleyraSms.API
import Kernel.External.SMS.KaleyraSms.Config
import Kernel.External.SMS.KaleyraSms.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common as B
import Servant.Client

sendOTPApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  KaleyraSmsCfg ->
  KaleyraSmsReq ->
  m KaleyraSmsRes
sendOTPApi kaleyraSmsCfg req = do
  apiKey_ <- decrypt kaleyraSmsCfg.apiKey
  sid_ <- decrypt kaleyraSmsCfg.sid
  let eulerClient = ET.client (Proxy @KaleyraSmsAPI)
  res <-
    callAPI
      kaleyraSmsCfg.url
      (eulerClient sid_ apiKey_ req)
      "sendOTPApi"
      (Proxy @KaleyraSmsAPI)
      >>= checkKaleyraError kaleyraSmsCfg.url
  pure res

checkKaleyraError :: (MonadThrow m, B.Log m) => BaseUrl -> Either ClientError KaleyraSmsRes -> m KaleyraSmsRes
checkKaleyraError url res =
  fromEitherM (kaleyraOptError url) res >>= validateResponseStatus

validateResponseStatus :: (MonadThrow m, B.Log m) => KaleyraSmsRes -> m KaleyraSmsRes
validateResponseStatus (KaleyraSmsSuccess res) = pure (KaleyraSmsSuccess res)
validateResponseStatus (KaleyraSmsError err) = do
  B.logError $ "Kaleyra SMS error: code=" <> err.code <> ", message=" <> err.message
  throwError $ KaleyraUnknownServerError err.message

kaleyraOptError :: BaseUrl -> ClientError -> ExternalAPICallError
kaleyraOptError = ExternalAPICallError (Just "KALEYRA_SMS_API_ERROR")
