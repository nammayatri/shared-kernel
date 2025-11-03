{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Digilocker.Flow where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as DT
import qualified EulerHS.Language as L
import EulerHS.Types (EulerClient, client)
import Kernel.External.Encryption
import Kernel.External.SharedLogic.DigiLocker.Error (parseDigiLockerErrorFromResponse)
import qualified Kernel.External.Verification.Digilocker.Types as DigiTypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Utils.Common
import Kernel.Utils.Servant.BaseUrl (showBaseUrlText)
import qualified Network.HTTP.Media as M
import Network.HTTP.Types (Status (..))
import Servant hiding (OctetStream, throwError)
import Servant.Client.Core (ClientError (..), ResponseF (..))

data OctetStream deriving (Typeable)

instance Accept OctetStream where
  contentTypes _ = pure $ "application" M.// "octet-stream"

instance MimeRender OctetStream BSL.ByteString where
  mimeRender _ = \x -> x

instance MimeUnrender OctetStream BSL.ByteString where
  mimeUnrender _ = pure . (\x -> x)

type DigiLockerXmlAPI =
  "public"
    :> "oauth2"
    :> "1"
    :> "xml"
    :> Capture "uri" Text
    :> Header "Authorization" Text
    :> Get '[PlainText] Text

type DigiLockerFileAPI =
  "public"
    :> "oauth2"
    :> "1"
    :> "file"
    :> Capture "uri" Text
    :> Header "Authorization" Text
    :> Get '[OctetStream] BSL.ByteString

xmlClient :: Text -> Maybe Text -> EulerClient Text
xmlClient uri authHeader = client (Proxy :: Proxy DigiLockerXmlAPI) uri authHeader

fileClient :: Text -> Maybe Text -> EulerClient BSL.ByteString
fileClient uri authHeader = client (Proxy :: Proxy DigiLockerFileAPI) uri authHeader

getXml ::
  ( HasCallStack,
    EncFlow m r,
    MonadFlow m,
    CoreMetrics m
  ) =>
  DigiTypes.DigiLockerCfg ->
  Text ->
  Text ->
  m Text
getXml cfg accessToken uri = do
  let authHeader = "Bearer " <> accessToken
  callAPI' Nothing cfg.url (xmlClient uri (Just authHeader)) "DGL-XML-API" (Proxy @DigiLockerXmlAPI)
    >>= checkDigiLockerResponse cfg.url

getFile ::
  ( HasCallStack,
    EncFlow m r,
    MonadFlow m,
    CoreMetrics m,
    Log m
  ) =>
  DigiTypes.DigiLockerCfg ->
  Text ->
  Text ->
  m BSL.ByteString
getFile cfg accessToken uri = do
  let authHeader = "Bearer " <> accessToken
  logDebug $ "Calling DigiLocker File API for URI: " <> uri
  res <-
    measuringDuration (Metrics.addRequestLatency (showBaseUrlText cfg.url) "DGL-FILE-API") $
      L.callAPI' Nothing cfg.url (fileClient uri (Just authHeader))
  checkDigiLockerFileResponse cfg.url res

checkDigiLockerResponse ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    Log m
  ) =>
  BaseUrl ->
  Either ClientError Text ->
  m Text
checkDigiLockerResponse url resp = case resp of
  Left err@(FailureResponse _ (Response (Status {statusCode = code}) _ _ body)) -> do
    logError $ "DigiLocker XML API call failed with status code: " <> show code <> ", error: " <> show err
    let errorBody = BSL.toStrict body
        digiLockerErr = parseDigiLockerErrorFromResponse code errorBody
    throwError digiLockerErr
  Left err -> do
    logError $ "DigiLocker XML API call failed: " <> show err
    fromEitherM (digiLockerError url) (Left err)
  Right resp' -> do
    logDebug $ "DigiLocker XML Response: " <> show (DT.take 500 resp')
    return resp'

checkDigiLockerFileResponse ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    Log m
  ) =>
  BaseUrl ->
  Either ClientError BSL.ByteString ->
  m BSL.ByteString
checkDigiLockerFileResponse url resp = case resp of
  Left err@(FailureResponse _ (Response (Status {statusCode = code}) _ _ body)) -> do
    logError $ "DigiLocker File API call failed with status code: " <> show code <> ", error: " <> show err
    let errorBody = BSL.toStrict body
        digiLockerErr = parseDigiLockerErrorFromResponse code errorBody
    throwError digiLockerErr
  Left err -> do
    logError $ "DigiLocker File API call failed: " <> show err
    fromEitherM (digiLockerError url) (Left err)
  Right resp' -> do
    logDebug $ "DigiLocker File API call succeeded, file size: " <> show (BSL.length resp')
    return resp'

digiLockerError :: BaseUrl -> ClientError -> ExternalAPICallError
digiLockerError = ExternalAPICallError (Just "DIGILOCKER_API_ERROR")
