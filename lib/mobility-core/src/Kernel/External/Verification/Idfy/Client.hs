{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Idfy.Client
  ( verifyDLAsync,
    verifyRCAsync,
    validateImage,
    extractRCImage,
    extractDLImage,
    extractPanImage,
    extractGSTImage,
    getTask,
    VerifyDLAPI,
    VerifyRCAPI,
    ValidateImage,
    ExtractDLImage,
    ExtractPanImage,
    ExtractGSTImage,
    ExtractRCAPI,
  )
where

import qualified Data.Aeson as DA
import qualified Data.Text as DT
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Kernel.External.Verification.Idfy.Auth
import Kernel.External.Verification.Idfy.Config
import Kernel.External.Verification.Idfy.Types.Error
import Kernel.External.Verification.Idfy.Types.Request
import Kernel.External.Verification.Idfy.Types.Response
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common
import Kernel.Types.Error (IdfyCallError (..))
import Kernel.Utils.Common hiding (Error)
import qualified Kernel.Utils.Text as KUT
import Servant (Get, Header, JSON, Post, ReqBody, (:>))

type VerifyDLAPI =
  "v3" :> "tasks" :> "async" :> "verify_with_source" :> "ind_driving_license"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] DLVerificationRequest
    :> Post '[JSON] IdfySuccess

verifyDLAPI :: Proxy VerifyDLAPI
verifyDLAPI = Proxy

verifyDLAsync ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  DLVerificationRequest ->
  m IdfySuccess
verifyDLAsync apiKey accountId url req = callIdfyAPI url task "verifyDLAsync" verifyDLAPI
  where
    task =
      T.client
        verifyDLAPI
        (Just apiKey)
        (Just accountId)
        req

type VerifyRCAPI =
  "v3" :> "tasks" :> "async" :> "verify_with_source" :> "ind_rc_plus"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] RCVerificationRequest
    :> Post '[JSON] IdfySuccess

verifyRCAPI :: Proxy VerifyRCAPI
verifyRCAPI = Proxy

verifyRCAsync ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  RCVerificationRequest ->
  m IdfySuccess
verifyRCAsync apiKey accountId url req = callIdfyAPI url task "verifyRCAsync" verifyRCAPI
  where
    task =
      T.client
        verifyRCAPI
        (Just apiKey)
        (Just accountId)
        req

type ValidateImage =
  "v3" :> "tasks" :> "sync" :> "validate" :> "document"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] ImageValidateRequest
    :> Post '[JSON] ImageValidateResponse

validateImageAPI :: Proxy ValidateImage
validateImageAPI = Proxy

validateImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  ImageValidateRequest ->
  m ImageValidateResponse
validateImage apiKey accountId url req = callIdfyAPI url task "validateImage" validateImageAPI
  where
    task =
      T.client
        validateImageAPI
        (Just apiKey)
        (Just accountId)
        req

type ExtractRCAPI =
  "v3" :> "tasks" :> "sync" :> "extract" :> "ind_rc"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] ImageExtractRequest
    :> Post '[JSON] RCExtractResponse

extractRCAPI :: Proxy ExtractRCAPI
extractRCAPI = Proxy

extractRCImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  ImageExtractRequest ->
  m RCExtractResponse
extractRCImage apiKey accountId url req = callIdfyAPI url task "extractRCImage" extractRCAPI
  where
    task =
      T.client
        extractRCAPI
        (Just apiKey)
        (Just accountId)
        req

type ExtractDLImage =
  "v3" :> "tasks" :> "sync" :> "extract" :> "ind_driving_license"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] ImageExtractRequest
    :> Post '[JSON] DLExtractResponse

extractDLAPI :: Proxy ExtractDLImage
extractDLAPI = Proxy

extractDLImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  ImageExtractRequest ->
  m DLExtractResponse
extractDLImage apiKey accountId url req = callIdfyAPI url task "extractDLImage" extractDLAPI
  where
    task =
      T.client
        extractDLAPI
        (Just apiKey)
        (Just accountId)
        req

type ExtractPanImage =
  "v3" :> "tasks" :> "sync" :> "extract" :> "ind_pan"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] ImageExtractRequest
    :> Post '[JSON] PanExtractionResponse

extractPanAPI :: Proxy ExtractPanImage
extractPanAPI = Proxy

extractPanImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  ImageExtractRequest ->
  m PanExtractionResponse
extractPanImage apiKey accountId url req = callIdfyAPI url task "extractPanImage" extractPanAPI
  where
    task =
      T.client
        extractPanAPI
        (Just apiKey)
        (Just accountId)
        req

type ExtractGSTImage =
  "v3" :> "tasks" :> "sync" :> "extract" :> "ind_gst_certificate"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] ImageExtractRequest
    :> Post '[JSON] GSTExtractionResponse

extractGSTAPI :: Proxy ExtractGSTImage
extractGSTAPI = Proxy

extractGSTImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  ImageExtractRequest ->
  m GSTExtractionResponse
extractGSTImage apiKey accountId url req = callIdfyAPI url task "extractGSTImage" extractGSTAPI
  where
    task =
      T.client
        extractGSTAPI
        (Just apiKey)
        (Just accountId)
        req

type GetTaskAPI =
  "v3" :> "tasks"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> MandatoryQueryParam "request_id" Text
    :> Get '[JSON] DA.Value

getTaskApi :: Proxy GetTaskAPI
getTaskApi = Proxy

getTask ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  Text ->
  m (VerificationResponse, Text)
getTask apiKey accountId url request_id = do
  (resp :: DA.Value) <- callIdfyAPI url task "getTask" getTaskApi
  convertValueToRespType resp <&> (,KUT.encodeToText resp)
  where
    task =
      T.client
        getTaskApi
        (Just apiKey)
        (Just accountId)
        request_id
    convertValueToRespType :: (MonadThrow m, Log m) => DA.Value -> m VerificationResponse
    convertValueToRespType rsp = case DA.fromJSON rsp of
      DA.Error err -> throwError $ IdfyCallError ("Could not parse Idfy getTask resp. Reason: " <> DT.pack err <> "Resp: " <> show rsp)
      DA.Success pyload -> return pyload

callIdfyAPI :: CallAPI env api res
callIdfyAPI = callApiUnwrappingApiError (identity @IdfyError) (Just $ T.ManagerSelector idfyHttpManagerKey) (Just "IDFY_ERROR") Nothing
