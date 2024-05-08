module Kernel.External.Verification.HyperVerge.Flow where

import EulerHS.Types (EulerClient, client)
import Kernel.External.Common.HyperVerge.HyperVergeErrors
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Verification.HyperVerge.Types as HyperVergeTypes
import Kernel.Prelude hiding (error)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Utils.Common
import Servant hiding (throwError)
import Servant.Client.Core (ClientError)

type HyperVergeRcValidationAPI =
  "v1"
    :> "fetchDetailedRC"
    :> Header "appid" Text
    :> Header "appKey" Text
    :> ReqBody '[JSON] HyperVergeTypes.HyperVergeRCValidationReq
    :> Post '[JSON] HyperVergeTypes.HyperVergeRCValidationResp

rcValidationClient :: Maybe Text -> Maybe Text -> HyperVergeTypes.HyperVergeRCValidationReq -> EulerClient HyperVergeTypes.HyperVergeRCValidationResp
rcValidationClient = client (Proxy :: Proxy HyperVergeRcValidationAPI)

validateRC ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    EncFlow m r
  ) =>
  HyperVergeTypes.HyperVergeConfig ->
  HyperVergeTypes.HyperVergeRCValidationReq ->
  m HyperVergeTypes.HyperVergeRCValidationResp
validateRC cfg req = do
  decrypt cfg.appKey >>= \key -> callAPI cfg.url (rcValidationClient (Just cfg.appId) (Just key) req) "HV-RcValidation-API" (Proxy @HyperVergeRcValidationAPI) >>= checkHyperVergeRcValidationResponse cfg.url

checkHyperVergeRcValidationResponse ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Either ClientError HyperVergeTypes.HyperVergeRCValidationResp ->
  m HyperVergeTypes.HyperVergeRCValidationResp
checkHyperVergeRcValidationResponse url resp = fromEitherM (hyperVergeError url) resp >>= validateHyperVergeRcValidationResponse

hyperVergeError :: BaseUrl -> ClientError -> ExternalAPICallError
hyperVergeError = ExternalAPICallError (Just "HYPERVERGE_API_ERROR")

validateHyperVergeRcValidationResponse :: (MonadThrow m, Log m) => HyperVergeTypes.HyperVergeRCValidationResp -> m HyperVergeTypes.HyperVergeRCValidationResp
validateHyperVergeRcValidationResponse resp = do
  logDebug $ "HyperVerge Rc Validation Response: " <> show resp
  case resp.statusCode of
    "401" -> throwError $ HVUnauthorizedError
    "400" -> throwError $ HVBadRequestError (geterror $ fromMaybe (Left "Null") resp.error)
    "200" -> return resp
    _ -> throwError $ HVError ("The response from HV is : " <> show resp)
  where
    geterror err = case err of
      Left e -> e
      Right e -> e.error.message
