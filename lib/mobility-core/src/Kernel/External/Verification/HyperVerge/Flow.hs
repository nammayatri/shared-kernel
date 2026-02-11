{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.HyperVerge.Flow where

import qualified Data.Aeson as DA
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
import qualified Kernel.Utils.Text as KUT
import Servant hiding (throwError)
import Servant.Client.Core

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
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
hyperVergeError = ExternalAPICallError (Just "HYPERVERGE_API_ERROR")

validateHyperVergeSdkVerificationResponse :: (MonadThrow m, Log m) => HyperVergeTypes.HyperVergeSdkVerificationRes -> m HyperVergeTypes.HyperVergeSdkVerificationRes
validateHyperVergeSdkVerificationResponse resp = do
  logDebug $ "HyperVerge SDK Verification Response: " <> show resp
  case resp.statusCode of
    Just 401 -> throwError HVUnauthorizedError
    Just 400 -> throwError $ HVBadRequestError (fromMaybe "No Message found in resp or Failed to parse error !!!!" $ join (resp.result <&> (.error)))
    Just 422 -> throwError $ HVBadInputError (fromMaybe "No Message found in resp or Failed to parse error !!!!" $ join (resp.result <&> (.error)))
    Just 200 -> return resp
    _ -> throwError $ HVError ("The response from HV is : " <> show resp)

type VerifyRCAsyncAPI =
  "v1"
    :> "async"
    :> "RCVerification"
    :> Header "appId" Text
    :> Header "appKey" Text
    :> Header "transactionId" Text
    :> ReqBody '[JSON] HyperVergeTypes.VerifyRCAsyncReq
    :> Post '[JSON] HyperVergeTypes.VerifyRCAsyncResp

rcAsyncVerificationClient :: Maybe Text -> Maybe Text -> Maybe Text -> HyperVergeTypes.VerifyRCAsyncReq -> EulerClient HyperVergeTypes.VerifyRCAsyncResp
rcAsyncVerificationClient = client (Proxy :: Proxy VerifyRCAsyncAPI)

verifyRCAsync ::
  ( HasCallStack,
    EncFlow m r,
    MonadFlow m,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  HyperVergeTypes.HyperVergeRCDLVerificationConfig ->
  Text ->
  HyperVergeTypes.VerifyRCAsyncReq ->
  m HyperVergeTypes.VerifyRCAsyncResp
verifyRCAsync cfg transactionId req = do
  decrypt cfg.appKey >>= \key -> callAPI' (Just $ ManagerSelector $ DT.pack hyperVergeHttpManagerKey) cfg.url (rcAsyncVerificationClient (Just cfg.appId) (Just key) (Just transactionId) req) "HV-RC_ASYNC_VERIFICATION-API" (Proxy @VerifyRCAsyncAPI) >>= checkHyperVergeRCAsyncVerificationResp cfg.url

checkHyperVergeRCAsyncVerificationResp ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Either ClientError HyperVergeTypes.VerifyRCAsyncResp ->
  m HyperVergeTypes.VerifyRCAsyncResp
checkHyperVergeRCAsyncVerificationResp url resp = fromEitherM (hyperVergeError url) resp >>= validateHyperVergeRCAsyncVerificationResponse

validateHyperVergeRCAsyncVerificationResponse :: (MonadThrow m, Log m) => HyperVergeTypes.VerifyRCAsyncResp -> m HyperVergeTypes.VerifyRCAsyncResp
validateHyperVergeRCAsyncVerificationResponse resp = do
  logDebug $ "HyperVerge RC Verification Response: " <> show resp
  case resp.statusCode of
    401 -> throwError HVUnauthorizedError
    400 -> throwError $ HVBadRequestError (fromMaybe "No Message found in resp or Failed to parse error !!!!" resp.message)
    200 -> return resp
    _ -> throwError $ HVError ("The response from HV is : " <> show resp)

type VerifyDLAsyncAPI =
  "v1"
    :> "async"
    :> "checkDL"
    :> Header "appId" Text
    :> Header "appKey" Text
    :> Header "transactionId" Text
    :> ReqBody '[JSON] HyperVergeTypes.HyperVergeDLVerificationReq
    :> Post '[JSON] HyperVergeTypes.HyperVergeDLVerificationResp

dlAsyncVerificationClient :: Maybe Text -> Maybe Text -> Maybe Text -> HyperVergeTypes.HyperVergeDLVerificationReq -> EulerClient HyperVergeTypes.HyperVergeDLVerificationResp
dlAsyncVerificationClient = client (Proxy :: Proxy VerifyDLAsyncAPI)

verifyDLAsync ::
  ( HasCallStack,
    EncFlow m r,
    MonadFlow m,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  HyperVergeTypes.HyperVergeRCDLVerificationConfig ->
  Text ->
  HyperVergeTypes.HyperVergeDLVerificationReq ->
  m HyperVergeTypes.HyperVergeDLVerificationResp
verifyDLAsync cfg transactionId req = do
  decrypt cfg.appKey >>= \key -> callAPI' (Just $ ManagerSelector $ DT.pack hyperVergeHttpManagerKey) cfg.url (dlAsyncVerificationClient (Just cfg.appId) (Just key) (Just transactionId) req) "HV-DL_ASYNC_VERIFICATION-API" (Proxy @VerifyDLAsyncAPI) >>= checkHyperVergeDLAsyncVerificationResp cfg.url

checkHyperVergeDLAsyncVerificationResp ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Either ClientError HyperVergeTypes.HyperVergeDLVerificationResp ->
  m HyperVergeTypes.HyperVergeDLVerificationResp
checkHyperVergeDLAsyncVerificationResp url resp = fromEitherM (hyperVergeError url) resp >>= validateHyperVergeDLAsyncVerificationResponse

validateHyperVergeDLAsyncVerificationResponse :: (MonadThrow m, Log m) => HyperVergeTypes.HyperVergeDLVerificationResp -> m HyperVergeTypes.HyperVergeDLVerificationResp
validateHyperVergeDLAsyncVerificationResponse resp = do
  logDebug $ "HyperVerge DL Verification Response: " <> show resp
  case resp.statusCode of
    401 -> throwError HVUnauthorizedError
    400 -> throwError $ HVBadRequestError (fromMaybe "No Message found in resp or Failed to parse error !!!!" resp.error)
    200 -> return resp
    _ -> throwError $ HVError ("The response from HV is : " <> show resp)

type GetVerificationStatusAPI =
  "v1"
    :> "async"
    :> Header "appId" Text
    :> Header "appKey" Text
    :> Capture "workflowId" Text
    :> MandatoryQueryParam "requestId" Text
    :> Get '[JSON] DA.Value

getVerificationStatusClient :: Maybe Text -> Maybe Text -> Text -> Text -> EulerClient DA.Value
getVerificationStatusClient = client (Proxy :: Proxy GetVerificationStatusAPI)

getVerificationStatus ::
  ( HasCallStack,
    EncFlow m r,
    MonadFlow m,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  HyperVergeTypes.HyperVergeRCDLVerificationConfig ->
  Text ->
  Text ->
  m (HyperVergeTypes.GetVerificationStatusResp, Text)
getVerificationStatus cfg workflowId reqId = do
  decrypt cfg.appKey >>= \key -> callAPI' (Just $ ManagerSelector $ DT.pack hyperVergeHttpManagerKey) cfg.url (getVerificationStatusClient (Just cfg.appId) (Just key) workflowId reqId) "HV-GET_VERIFICATION_RESULT-API" (Proxy @GetVerificationStatusAPI) >>= checkHyperVergeGetVerificationStatusResp cfg.url

checkHyperVergeGetVerificationStatusResp ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Either ClientError DA.Value ->
  m (HyperVergeTypes.GetVerificationStatusResp, Text)
checkHyperVergeGetVerificationStatusResp url resp = fromEitherM (hyperVergeError url) resp >>= \rsp -> (,KUT.encodeToText rsp) <$> (validateHyperVergeGetVerificationStatusResp =<< convertValueToRespType rsp)
  where
    convertValueToRespType :: (MonadThrow m, Log m) => DA.Value -> m HyperVergeTypes.GetVerificationStatusResp
    convertValueToRespType rsp = case DA.fromJSON rsp of
      DA.Error err -> throwError $ HVError ("Could not parse HyperVerge getVerificationStatus resp. Reason: " <> DT.pack err <> "Resp: " <> show rsp)
      DA.Success pyload -> return pyload

validateHyperVergeGetVerificationStatusResp :: (MonadThrow m, Log m) => HyperVergeTypes.GetVerificationStatusResp -> m HyperVergeTypes.GetVerificationStatusResp
validateHyperVergeGetVerificationStatusResp resp = do
  logDebug $ "HyperVerge getVerificationStatus Response: " <> show resp

  -- Check if it's the new format
  case resp.verificationStatuses of
    Just statuses -> do
      -- New format: validate verificationStatuses array
      if null statuses
        then throwError $ HVError "No verification statuses found in response"
        else do
          -- Optionally check if all statuses are "success"
          let hasFailures = any (\s -> s.status /= "success") statuses
          when hasFailures $
            logWarning $ "Some verification statuses are not successful: " <> show statuses
          return resp
    Nothing -> do
      -- Old format: validate statusCode
      case resp.statusCode of
        Just 401 -> throwError HVUnauthorizedError
        Just 400 -> throwError $ HVBadRequestError (fromMaybe "No Message found in resp or Failed to parse error !!!!" resp.message)
        Just 200 -> if isNothing resp.result then throwError (HVMissingPayloadError $ fromMaybe ("Unknown reason !!!!! Resp : " <> show resp) resp.message) else return resp
        _ -> throwError $ HVError ("The response from HV is : " <> show resp)
