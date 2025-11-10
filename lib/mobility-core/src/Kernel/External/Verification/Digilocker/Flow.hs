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
import qualified Data.Text.Encoding as TE
import qualified EulerHS.Language as L
import EulerHS.Types (EulerClient, client)
import Kernel.External.Encryption
import Kernel.External.SharedLogic.DigiLocker.Error (DigiLockerError (..), parseDigiLockerErrorFromResponse)
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

data ApplicationXML deriving (Typeable)

data OctetStream deriving (Typeable)

data PDF deriving (Typeable)

instance Accept ApplicationXML where
  contentTypes _ = pure $ "application" M.// "xml"

instance MimeUnrender ApplicationXML Text where
  mimeUnrender _ bs =
    case TE.decodeUtf8' (BSL.toStrict bs) of
      Left err -> Left (show err)
      Right text -> Right text

instance Accept OctetStream where
  contentTypes _ = pure $ "application" M.// "octet-stream"

instance MimeRender OctetStream BSL.ByteString where
  mimeRender _ = \x -> x

instance MimeUnrender OctetStream BSL.ByteString where
  mimeUnrender _ = pure . (\x -> x)

instance Accept PDF where
  contentTypes _ = pure $ "application" M.// "pdf"

instance MimeRender PDF BSL.ByteString where
  mimeRender _ = \x -> x

instance MimeUnrender PDF BSL.ByteString where
  mimeUnrender _ = pure . (\x -> x)

type DigiLockerXmlAPI =
  "public"
    :> "oauth2"
    :> "1"
    :> "xml"
    :> Capture "uri" Text
    :> Header "Authorization" Text
    :> Get '[ApplicationXML, PlainText] Text

type DigiLockerFileAPI =
  "public"
    :> "oauth2"
    :> "1"
    :> "file"
    :> Capture "uri" Text
    :> Header "Authorization" Text
    :> Get '[OctetStream, PDF] BSL.ByteString

type DigiLockerPullDrivingLicenseAPI =
  "public"
    :> "oauth2"
    :> "1"
    :> "pull"
    :> "pulldocument"
    :> Header "Authorization" Text
    :> ReqBody '[FormUrlEncoded] DigiTypes.DigiLockerPullDrivingLicenseRequest
    :> Post '[JSON] DigiTypes.DigiLockerPullDocumentResponse

type DigiLockerAadhaarXmlAPI =
  "public"
    :> "oauth2"
    :> "3"
    :> "xml"
    :> "eaadhaar"
    :> Header "Authorization" Text
    :> Get '[ApplicationXML, PlainText] Text

xmlClient :: Text -> Maybe Text -> EulerClient Text
xmlClient uri authHeader = client (Proxy :: Proxy DigiLockerXmlAPI) uri authHeader

fileClient :: Text -> Maybe Text -> EulerClient BSL.ByteString
fileClient uri authHeader = client (Proxy :: Proxy DigiLockerFileAPI) uri authHeader

pullDrivingLicenseClient :: Maybe Text -> DigiTypes.DigiLockerPullDrivingLicenseRequest -> EulerClient DigiTypes.DigiLockerPullDocumentResponse
pullDrivingLicenseClient authHeader = client (Proxy :: Proxy DigiLockerPullDrivingLicenseAPI) authHeader

aadhaarXmlClient :: Maybe Text -> EulerClient Text
aadhaarXmlClient authHeader = client (Proxy :: Proxy DigiLockerAadhaarXmlAPI) authHeader

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

pullDrivingLicense ::
  ( HasCallStack,
    EncFlow m r,
    MonadFlow m,
    CoreMetrics m,
    Log m
  ) =>
  DigiTypes.DigiLockerCfg ->
  Text ->
  DigiTypes.DigiLockerPullDrivingLicenseRequest ->
  m DigiTypes.DigiLockerPullDocumentResponse
pullDrivingLicense cfg accessToken req = do
  let authHeader = "Bearer " <> accessToken
  logInfo $ "Calling DigiLocker Pull Driving License API for orgid: " <> req.orgid <> ", doctype: " <> req.doctype <> ", dlno: " <> req.dlno
  logDebug $ "DigiLocker Pull Driving License request details: " <> show req
  res <-
    callAPI
      cfg.url
      (pullDrivingLicenseClient (Just authHeader) req)
      "DGL-PULL-DRIVING-LICENSE-API"
      (Proxy @DigiLockerPullDrivingLicenseAPI)
  checkDigiLockerPullDocumentResponse cfg.url res

checkDigiLockerPullDocumentResponse ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    Log m
  ) =>
  BaseUrl ->
  Either ClientError DigiTypes.DigiLockerPullDocumentResponse ->
  m DigiTypes.DigiLockerPullDocumentResponse
checkDigiLockerPullDocumentResponse url resp = case resp of
  Left err@(FailureResponse _ (Response (Status {statusCode = code}) _ _ body)) -> do
    logError $ "DigiLocker Pull Document API call failed with status code: " <> show code <> ", error: " <> show err
    let errorBody = BSL.toStrict body
        digiLockerErr = parseDigiLockerErrorFromResponse code errorBody
    throwError digiLockerErr
  Left err -> do
    logError $ "DigiLocker Pull Document API call failed: " <> show err
    fromEitherM (digiLockerError url) (Left err)
  Right resp' -> do
    logDebug $ "DigiLocker Pull Document Response: " <> show resp'
    validateDigiLockerPullDocumentResponse resp'

validateDigiLockerPullDocumentResponse ::
  ( HasCallStack,
    MonadThrow m,
    Log m
  ) =>
  DigiTypes.DigiLockerPullDocumentResponse ->
  m DigiTypes.DigiLockerPullDocumentResponse
validateDigiLockerPullDocumentResponse resp = do
  logDebug $ "DigiLocker Pull Document Response: " <> show resp
  when (DT.null resp.uri) $ do
    logError "DigiLocker Pull Document response validation failed: URI is missing"
    throwError $ DGLError "URI is missing in DigiLocker pull document response"
  logInfo $ "DigiLocker Pull Document API call succeeded, URI: " <> resp.uri
  return resp

getAadhaarXml ::
  ( HasCallStack,
    EncFlow m r,
    MonadFlow m,
    CoreMetrics m
  ) =>
  DigiTypes.DigiLockerCfg ->
  Text ->
  m Text
getAadhaarXml cfg accessToken = do
  let authHeader = "Bearer " <> accessToken
  callAPI' Nothing cfg.url (aadhaarXmlClient (Just authHeader)) "DGL-AADHAAR-XML-API" (Proxy @DigiLockerAadhaarXmlAPI)
    >>= checkDigiLockerResponse cfg.url

digiLockerError :: BaseUrl -> ClientError -> ExternalAPICallError
digiLockerError = ExternalAPICallError (Just "DIGILOCKER_API_ERROR")
