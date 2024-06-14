{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.HyperVerge.Flow where

import qualified Data.Text as DT
import EulerHS.Types (EulerClient, ManagerSelector (..), client)
import Kernel.External.Encryption
import Kernel.External.SharedLogic.HyperVerge.Error
import Kernel.External.SharedLogic.HyperVerge.Functions (hyperVergeHttpManagerKey)
import qualified Kernel.External.Verification.HyperVerge.Types as HyperVergeTypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Utils.Common
import Servant hiding (throwError)
import Servant.Client.Core (ClientError)

type HyperVergeSdkVerificationAPI =
  "v1"
    :> "output"
    :> Header "appId" Text
    :> Header "appKey" Text
    :> ReqBody '[JSON] HyperVergeTypes.HyperVergeSdkVerificationReq
    :> Post '[JSON] HyperVergeTypes.HyperVergeSdkVerificationRes

verificationClient :: Maybe Text -> Maybe Text -> HyperVergeTypes.HyperVergeSdkVerificationReq -> EulerClient HyperVergeTypes.HyperVergeSdkVerificationRes
verificationClient = client (Proxy :: Proxy HyperVergeSdkVerificationAPI)

verifySdkResp ::
  ( HasCallStack,
    EncFlow m r,
    MonadFlow m,
    CoreMetrics m
  ) =>
  HyperVergeTypes.HyperVergeVerificationCfg ->
  HyperVergeTypes.HyperVergeSdkVerificationReq ->
  m HyperVergeTypes.HyperVergeSdkVerificationRes
verifySdkResp cfg req = do
  decrypt cfg.appKey >>= \key -> callAPI' (Just $ ManagerSelector $ DT.pack hyperVergeHttpManagerKey) cfg.url (verificationClient (Just cfg.appId) (Just key) req) "HV-SDK_Verification-API" (Proxy @HyperVergeSdkVerificationAPI) >>= checkHyperVergeSdkVerificationResponse cfg.url

checkHyperVergeSdkVerificationResponse ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Either ClientError HyperVergeTypes.HyperVergeSdkVerificationRes ->
  m HyperVergeTypes.HyperVergeSdkVerificationRes
checkHyperVergeSdkVerificationResponse url resp = fromEitherM (hyperVergeError url) resp >>= validateHyperVergeSdkVerificationResponse

hyperVergeError :: BaseUrl -> ClientError -> ExternalAPICallError
hyperVergeError = ExternalAPICallError (Just "HYPERVERGE_SDK_VERIFICATION_API_ERROR")

validateHyperVergeSdkVerificationResponse :: (MonadThrow m, Log m) => HyperVergeTypes.HyperVergeSdkVerificationRes -> m HyperVergeTypes.HyperVergeSdkVerificationRes
validateHyperVergeSdkVerificationResponse resp = do
  logDebug $ "HyperVerge SDK Verification Response: " <> show resp
  case resp.statusCode of
    Just 401 -> throwError $ HVUnauthorizedError
    Just 400 -> throwError $ HVBadRequestError (fromMaybe "No Message found in resp or Failed to parse error !!!!" $ join (resp.result <&> (.error)))
    Just 422 -> throwError $ HVBadInputError (fromMaybe "No Message found in resp or Failed to parse error !!!!" $ join (resp.result <&> (.error)))
    Just 200 -> return resp
    _ -> throwError $ HVError ("The response from HV is : " <> show resp)
