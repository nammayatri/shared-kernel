{-# LANGUAGE DerivingVia #-}

module Kernel.External.Verification.HyperVerge.Flow where

import Data.ByteString.Lazy (ByteString)
import EulerHS.Types (EulerClient, client)
import Kernel.External.Verification.HyperVerge.Error
import Kernel.External.Verification.HyperVerge.Types
import Kernel.Prelude hiding (decodeUtf8)
import Kernel.ServantMultipart
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Utils.Common
import Servant hiding (throwError)
import Servant.Client.Core (ClientError)

type FaceValidationAPI =
  "v2"
    :> "faceAuth"
    :> Header "Content-Type" Text
    :> Header "accept" Text
    :> Header "appId" AppId
    :> Header "appKey" AppKey
    :> MultipartForm Tmp FaceValidationReq
    :> Post '[JSON] (Either FaceValidationRespFailure FaceValidationRespSuccess)

faceValidationClient ::
  Maybe Text ->
  Maybe Text ->
  Maybe AppId ->
  Maybe AppKey ->
  (ByteString, FaceValidationReq) ->
  EulerClient (Either FaceValidationRespFailure FaceValidationRespSuccess)
faceValidationClient = client (Proxy :: Proxy FaceValidationAPI)

validateFaceImage :: (MonadFlow m, CoreMetrics m) => BaseUrl -> AppId -> AppKey -> FaceValidationReq -> m FaceValidationRespSuccess
validateFaceImage url appId appKey req =
  callAPI url (faceValidationClient (Just "multipart/form-data") (Just "application/json") (Just appId) (Just appKey) ("", req)) "validateFaceImage" (Proxy @FaceValidationAPI)
    >>= checkHyperVergeError url

checkHyperVergeError :: (MonadThrow m, Log m) => BaseUrl -> Either ClientError (Either FaceValidationRespFailure FaceValidationRespSuccess) -> m FaceValidationRespSuccess
checkHyperVergeError url resp = fromEitherM (hyperVergeApiCallError url) resp >>= validateResponseStatus

hyperVergeApiCallError :: BaseUrl -> ClientError -> ExternalAPICallError
hyperVergeApiCallError = ExternalAPICallError (Just "HYPERVERGE_API_CALL_ERROR")

validateResponseStatus :: (MonadThrow m, Log m) => (Either FaceValidationRespFailure FaceValidationRespSuccess) -> m FaceValidationRespSuccess
validateResponseStatus (Left resp) = do
  case resp.statusCode of
    401 -> throwError InvalidCredentials
    429 -> throwError RequestsRateLimitExceeded
    500 -> throwError InternalServerError
    _ -> throwError (HyperVergeAPIError resp.err)
validateResponseStatus (Right resp) = return resp
