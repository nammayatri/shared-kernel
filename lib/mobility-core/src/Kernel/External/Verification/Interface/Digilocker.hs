{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Interface.Digilocker
  ( getXML,
    getFile,
    pullDrivingLicense,
  )
where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Kernel.External.SharedLogic.DigiLocker.Error (DigiLockerError (..))
import qualified Kernel.External.Verification.Digilocker.Flow as DigiFlow
import qualified Kernel.External.Verification.Digilocker.Types as DigiTypes
import qualified Kernel.External.Verification.Idfy.Types.Response as Idfy
import qualified Kernel.External.Verification.Interface.Types as InterfaceTypes
import Kernel.Prelude hiding (error)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common
import Text.XML (def, parseLBS)
import Text.XML.Cursor

-- | Get XML document from DigiLocker and parse it to return structured data
-- Takes the DigiLocker config, access token (Bearer token), and document URI
getXML ::
  ( CoreMetrics m,
    EncFlow m r,
    MonadFlow m
  ) =>
  DigiTypes.DigiLockerCfg ->
  Text ->
  Text ->
  m InterfaceTypes.GetTaskResp
getXML config accessToken uri = do
  xmlText <- DigiFlow.getXml config accessToken uri
  parseXmlToGetTaskResp xmlText

-- | Get file (PDF) from DigiLocker
-- Takes the DigiLocker config, access token (Bearer token), and document URI
-- Returns the file as ByteString for storage in DB
getFile ::
  ( CoreMetrics m,
    EncFlow m r,
    MonadFlow m
  ) =>
  DigiTypes.DigiLockerCfg ->
  Text ->
  Text ->
  m BSL.ByteString
getFile config accessToken uri = do
  DigiFlow.getFile config accessToken uri

-- | Pull driving license document from DigiLocker
-- Takes the DigiLocker config, access token (Bearer token), and pull driving license request
-- Returns the URI of the pulled document
pullDrivingLicense ::
  ( CoreMetrics m,
    EncFlow m r,
    MonadFlow m
  ) =>
  DigiTypes.DigiLockerCfg ->
  Text ->
  DigiTypes.DigiLockerPullDrivingLicenseRequest ->
  m DigiTypes.DigiLockerPullDocumentResponse
pullDrivingLicense config accessToken req = do
  DigiFlow.pullDrivingLicense config accessToken req

parseXmlToGetTaskResp ::
  ( MonadThrow m,
    Log m
  ) =>
  Text ->
  m InterfaceTypes.GetTaskResp
parseXmlToGetTaskResp xmlText = do
  doc <- case parseLBS def (TLE.encodeUtf8 $ TL.fromStrict xmlText) of
    Left err -> throwError $ DGLError $ "Failed to parse XML: " <> T.pack (show err)
    Right d -> pure d
  let cursor = fromDocument doc
  cert <- extractCertificate cursor
  case cert of
    PANCertificate panData -> parsePanToGetTaskResp panData
    DLCertificate dlData -> parseDLToGetTaskResp dlData
    AadhaarCertificate _aadhaarData -> throwError $ InternalError "Aadhaar XML parsing not yet implemented for GetTaskResp"

data CertificateType = PANCertificate PanData | DLCertificate DLData | AadhaarCertificate AadhaarData
  deriving (Show)

data PanData = PanData
  { panNumber :: Maybe Text,
    name :: Maybe Text,
    dob :: Maybe Text
  }
  deriving (Show)

data DLData = DLData
  { dlNumber :: Maybe Text,
    name :: Maybe Text,
    dob :: Maybe Text,
    issueDate :: Maybe Text,
    expiryDate :: Maybe Text,
    validFromDate :: Maybe Text,
    status :: Maybe Text,
    categories :: Maybe [CategoryData]
  }
  deriving (Show)

data CategoryData = CategoryData
  { code :: Maybe Text,
    abbreviation :: Maybe Text,
    description :: Maybe Text,
    issueDate :: Maybe Text
  }
  deriving (Show)

data AadhaarData = AadhaarData
  { name :: Maybe Text,
    dob :: Maybe Text,
    gender :: Maybe Text,
    address :: Maybe Text
  }
  deriving (Show)

extractCertificate :: (MonadThrow m, Log m) => Cursor -> m CertificateType
extractCertificate cursor = do
  let certType = listToMaybe $ laxAttribute "type" cursor
  case certType of
    Just "PANCR" -> PANCertificate <$> parsePanXML cursor
    Just "DRVLC" -> DLCertificate <$> parseDLXML cursor
    Just ct -> throwError $ DGLError ("Unknown certificate type: " <> ct)
    Nothing -> throwError $ DGLError "Certificate type attribute missing"

parsePanXML :: (MonadThrow m, Log m) => Cursor -> m PanData
parsePanXML cursor = do
  let panNum = listToMaybe $ (cursor $// element "CertificateData" &/ element "PAN") >>= laxAttribute "num"
      nameAttr = listToMaybe $ (cursor $// element "IssuedTo" &/ element "Person") >>= laxAttribute "name"
      dobAttr = listToMaybe $ (cursor $// element "IssuedTo" &/ element "Person") >>= laxAttribute "dob"

  return $ PanData {panNumber = panNum, name = nameAttr, dob = dobAttr}

parseDLXML :: (MonadThrow m, Log m) => Cursor -> m DLData
parseDLXML cursor = do
  let dlNumberAttr = listToMaybe $ laxAttribute "number" cursor
      issueDateAttr = listToMaybe $ laxAttribute "issueDate" cursor
      expiryDateAttr = listToMaybe $ laxAttribute "expiryDate" cursor
      validFromDateAttr = listToMaybe $ laxAttribute "validFromDate" cursor
      statusAttr = listToMaybe $ laxAttribute "status" cursor

      nameAttr = listToMaybe $ (cursor $// element "IssuedTo" &/ element "Person") >>= laxAttribute "name"
      dobAttr = listToMaybe $ (cursor $// element "IssuedTo" &/ element "Person") >>= laxAttribute "dob"

      categoryCursors = cursor $// element "CertificateData" &/ element "DrivingLicense" &/ element "Categories" &/ element "Category"
      categoryDataList = map parseCategory categoryCursors

  return $
    DLData
      { dlNumber = dlNumberAttr,
        name = nameAttr,
        dob = dobAttr,
        issueDate = issueDateAttr,
        expiryDate = expiryDateAttr,
        validFromDate = validFromDateAttr,
        status = statusAttr,
        categories = if null categoryDataList then Nothing else Just categoryDataList
      }

parseCategory :: Cursor -> CategoryData
parseCategory cursor =
  CategoryData
    { code = listToMaybe $ laxAttribute "code" cursor,
      abbreviation = listToMaybe $ laxAttribute "abbreviation" cursor,
      description = listToMaybe $ laxAttribute "description" cursor,
      issueDate = listToMaybe $ laxAttribute "issueDate" cursor
    }

parsePanToGetTaskResp :: (MonadThrow m, Log m) => PanData -> m InterfaceTypes.GetTaskResp
parsePanToGetTaskResp _panData = do
  -- PAN doesn't map to DLResp or RCResp, but we need to return something
  -- Based on the interface, PAN verification might not use GetTaskResp
  -- For now, throw an error or return a minimal response
  throwError $ InternalError "PAN certificate parsing to GetTaskResp not implemented - use extractPanImage instead"

parseDLToGetTaskResp :: (MonadThrow m, Log m) => DLData -> m InterfaceTypes.GetTaskResp
parseDLToGetTaskResp dlData = do
  let covs = fmap (map categoryToCovDetail) dlData.categories
  return $
    InterfaceTypes.DLResp $
      InterfaceTypes.DLVerificationOutputInterface
        { driverName = dlData.name,
          dob = dlData.dob,
          licenseNumber = dlData.dlNumber,
          nt_validity_from = dlData.validFromDate,
          nt_validity_to = dlData.expiryDate,
          t_validity_from = Nothing, -- Transport validity not in DL XML
          t_validity_to = Nothing,
          covs = covs,
          status = dlData.status,
          dateOfIssue = dlData.issueDate,
          message = Nothing
        }

categoryToCovDetail :: CategoryData -> Idfy.CovDetail
categoryToCovDetail cat =
  Idfy.CovDetail
    { category = cat.code,
      issue_date = cat.issueDate,
      cov = fromMaybe "" cat.abbreviation
    }
