{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Tokenize.Digilocker.Flow where

import qualified Data.Text as T
import EulerHS.Types (EulerClient, client)
import Kernel.External.SharedLogic.DigiLocker.Error (DigiLockerError (..))
import qualified Kernel.External.Tokenize.Digilocker.Types as DLT
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Utils.Common
import Network.HTTP.Types (Status (..))
import Servant hiding (throwError)
import Servant.Client.Core (ClientError (..), ResponseF (..))

type DigilockerTokenizeAPI =
  "public"
    :> "oauth2"
    :> "1"
    :> "token"
    :> ReqBody '[FormUrlEncoded] DLT.DigilockerTokenizeRequest
    :> Post '[JSON] DLT.DigilockerTokenizeResponse

tokenizeClient :: DLT.DigilockerTokenizeRequest -> EulerClient DLT.DigilockerTokenizeResponse
tokenizeClient = client (Proxy :: Proxy DigilockerTokenizeAPI)

digilockerError :: BaseUrl -> ClientError -> ExternalAPICallError
digilockerError baseUrl clientError =
  ExternalAPICallError
    { errCode = Just "DIGILOCKER_TOKENIZE_API_ERROR",
      baseUrl = baseUrl,
      clientError = clientError
    }

checkDigilockerTokenizeResponse ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    Log m
  ) =>
  BaseUrl ->
  Either ClientError DLT.DigilockerTokenizeResponse ->
  m DLT.DigilockerTokenizeResponse
checkDigilockerTokenizeResponse baseUrl resp =
  case resp of
    Left err@(FailureResponse _ (Response (Status {statusCode = code}) _ _ _)) -> do
      let errMsg = "No error message found in response or failed to parse error"
      logError $ "DigiLocker tokenize API call failed with status code: " <> show code <> ", error: " <> show err
      case code of
        401 -> do
          logError "DigiLocker tokenize failed: Unauthorized (401)"
          throwError DLUnauthorizedError
        400 -> do
          logError $ "DigiLocker tokenize failed: Bad Request (400) - " <> errMsg
          throwError $ DLBadRequestError errMsg
        _ -> do
          logError $ "DigiLocker tokenize failed with unexpected status code: " <> show code
          fromEitherM (digilockerError baseUrl) (Left err)
    Left err -> do
      logError $ "DigiLocker tokenize API call failed: " <> show err
      fromEitherM (digilockerError baseUrl) (Left err)
    Right resp' -> do
      logInfo "DigiLocker tokenize API call succeeded"
      validateDigilockerTokenizeResponse resp'

validateDigilockerTokenizeResponse ::
  ( MonadThrow m,
    Log m
  ) =>
  DLT.DigilockerTokenizeResponse ->
  m DLT.DigilockerTokenizeResponse
validateDigilockerTokenizeResponse resp = do
  logDebug $ "DigiLocker Tokenize Response: " <> show resp
  when (T.null resp.access_token) $ do
    logError "DigiLocker tokenize response validation failed: Access token is missing"
    throwError $ DLError "Access token is missing in DigiLocker response"
  logInfo "DigiLocker tokenize response validated successfully"
  return resp

tokenize ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    Log m
  ) =>
  BaseUrl ->
  DLT.DigilockerTokenizeRequest ->
  m DLT.DigilockerTokenizeResponse
tokenize baseUrl req = do
  logInfo $ "Starting DigiLocker tokenize request to baseUrl: " <> showBaseUrl baseUrl
  logDebug $ "DigiLocker tokenize request details: " <> show req
  res <- callAPI baseUrl (tokenizeClient req) "DIGILOCKER_TOKENIZE" (Proxy @DigilockerTokenizeAPI)
  resp <- checkDigilockerTokenizeResponse baseUrl res
  logInfo "DigiLocker tokenize completed successfully"
  return resp
