{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Interface.Digilocker
  ( fetchAndExtractVerifiedDL,
    getFile,
    pullDrivingLicense,
    fetchAndExtractVerifiedPan,
    fetchAndExtractVerifiedAadhaar,
    getVerifiedAadhaarXML,
  )
where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Kernel.External.SharedLogic.DigiLocker.Error (DigiLockerError (..))
import qualified Kernel.External.Verification.Digilocker.Flow as DigiFlow
import qualified Kernel.External.Verification.Digilocker.Types as DigiTypes
import qualified Kernel.External.Verification.Interface.Types as InterfaceTypes
import Kernel.Prelude hiding (error)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Text.XML (def, parseLBS)
import Text.XML.Cursor

-- | Fetch and extract verified Driving License from DigiLocker XML
-- Takes the DigiLocker config, access token (Bearer token), and document URI
-- Returns ExtractedDigiLockerDLResp
fetchAndExtractVerifiedDL ::
  ( CoreMetrics m,
    EncFlow m r,
    MonadFlow m
  ) =>
  DigiTypes.DigiLockerCfg ->
  Text ->
  Text ->
  m InterfaceTypes.ExtractedDigiLockerDLResp
fetchAndExtractVerifiedDL config accessToken uri = do
  xmlText <- DigiFlow.getXml config accessToken uri
  doc <- case parseLBS def (TLE.encodeUtf8 $ TL.fromStrict xmlText) of
    Left err -> throwError $ DGLError $ "Failed to parse XML: " <> T.pack (show err)
    Right d -> pure d
  let cursor = fromDocument doc
  cert <- extractCertificate cursor
  dlFlow <- case cert of
    DLCertificate dlData -> parseDLToDLFlow dlData
    _ -> throwError $ DGLError "Expected Driving License certificate, but got different certificate type"
  return $ InterfaceTypes.ExtractedDigiLockerDLResp {extractedDL = Just dlFlow}

-- | Get file (PDF) from DigiLocker
-- Takes the DigiLocker config, access token (Bearer token), and document URI
-- Returns the file as ByteString for storage in DB
getFile ::
  ( CoreMetrics m,
    EncFlow m r,
    MonadFlow m,
    Log m
  ) =>
  DigiTypes.DigiLockerCfg ->
  Text ->
  Text ->
  m BSL.ByteString
getFile config accessToken uri = do
  logInfo $
    "Interface.Digilocker.getFile -> calling Flow.getFile | uri="
      <> uri
      <> ", accessTokenPresent="
      <> show (not $ T.null accessToken)
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

data CertificateType = PANCertificate PanData | DLCertificate DLData | AadhaarCertificate AadhaarData
  deriving (Show)

data PanData = PanData
  { panNumber :: Maybe Text,
    name :: Maybe Text,
    dob :: Maybe Text,
    dateOfIssue :: Maybe Text,
    validFromDate :: Maybe Text,
    status :: Maybe Text,
    panType :: Maybe Text
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
  { aadhaarNumber :: Maybe Text,
    name :: Maybe Text,
    dob :: Maybe Text,
    gender :: Maybe Text,
    fathersName :: Maybe Text,
    addressLine1 :: Maybe Text,
    addressLine2 :: Maybe Text,
    district :: Maybe Text,
    state :: Maybe Text,
    pincode :: Maybe Text,
    country :: Maybe Text,
    photo :: Maybe Text
  }
  deriving (Show)

extractCertificate :: (MonadThrow m, Log m) => Cursor -> m CertificateType
extractCertificate cursor = do
  let certType = listToMaybe $ laxAttribute "type" cursor
  case certType of
    Just "PANCR" -> PANCertificate <$> parsePanXML cursor
    Just "DRVLC" -> DLCertificate <$> parseDLXML cursor
    Just ct -> throwError $ DGLError ("Unknown certificate type: " <> ct)
    Nothing ->
      -- Check if it's an Aadhaar certificate (KycRes structure)
      if not (null $ cursor $// element "CertificateData" &/ element "KycRes")
        then AadhaarCertificate <$> parseAadhaarXML cursor
        else throwError $ DGLError "Certificate type attribute missing"

parsePanXML :: (MonadThrow m, Log m) => Cursor -> m PanData
parsePanXML cursor = do
  let panNumFromCert = listToMaybe $ laxAttribute "number" cursor
      panNumFromElement = listToMaybe $ (cursor $// element "CertificateData" &/ element "PAN") >>= laxAttribute "num"
      panNum = panNumFromElement <|> panNumFromCert -- Try element first, fallback to certificate attribute
      nameAttr = listToMaybe $ (cursor $// element "IssuedTo" &/ element "Person") >>= laxAttribute "name"
      dobAttr = listToMaybe $ (cursor $// element "IssuedTo" &/ element "Person") >>= laxAttribute "dob"
      dateOfIssueAttr = listToMaybe $ laxAttribute "issueDate" cursor
      validFromDateAttr = listToMaybe $ laxAttribute "validFromDate" cursor
      statusAttr = listToMaybe $ laxAttribute "status" cursor
      -- PAN type can be determined from PAN number format or other attributes
      panTypeAttr = Nothing -- Could be extracted from PAN number pattern if needed
  return $
    PanData
      { panNumber = panNum,
        name = nameAttr,
        dob = dobAttr,
        dateOfIssue = dateOfIssueAttr,
        validFromDate = validFromDateAttr,
        status = statusAttr,
        panType = panTypeAttr
      }

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

parseAadhaarXML :: (MonadThrow m, Log m) => Cursor -> m AadhaarData
parseAadhaarXML cursor = do
  let uidDataCursors = cursor $// element "CertificateData" &/ element "KycRes" &/ element "UidData"

      aadhaarNum = listToMaybe $ uidDataCursors >>= laxAttribute "uid"

      poiCursors = cursor $// element "CertificateData" &/ element "KycRes" &/ element "UidData" &/ element "Poi"
      nameAttr = listToMaybe $ poiCursors >>= laxAttribute "name"
      dobAttr = listToMaybe $ poiCursors >>= laxAttribute "dob"
      genderAttr = listToMaybe $ poiCursors >>= laxAttribute "gender"

      poaCursors = cursor $// element "CertificateData" &/ element "KycRes" &/ element "UidData" &/ element "Poa"
      fathersNameAttr = listToMaybe $ poaCursors >>= laxAttribute "co"
      houseAttr = listToMaybe $ poaCursors >>= laxAttribute "house"
      streetAttr = listToMaybe $ poaCursors >>= laxAttribute "street"
      locAttr = listToMaybe $ poaCursors >>= laxAttribute "loc"
      vtcAttr = listToMaybe $ poaCursors >>= laxAttribute "vtc"
      districtAttr = listToMaybe $ poaCursors >>= laxAttribute "dist"
      stateAttr = listToMaybe $ poaCursors >>= laxAttribute "state"
      pincodeAttr = listToMaybe $ poaCursors >>= laxAttribute "pc"
      countryAttr = listToMaybe $ poaCursors >>= laxAttribute "country"

      photoCursors = cursor $// element "CertificateData" &/ element "KycRes" &/ element "UidData" &/ element "Pht"
      photoAttr = listToMaybe $ photoCursors >>= content

      -- Combine address components
      addressLine1Parts = Maybe.catMaybes [houseAttr, streetAttr, locAttr]
      addressLine1 = if null addressLine1Parts then Nothing else Just $ T.intercalate ", " addressLine1Parts
      addressLine2 = vtcAttr

  return $
    AadhaarData
      { aadhaarNumber = aadhaarNum,
        name = nameAttr,
        dob = dobAttr,
        gender = genderAttr,
        fathersName = fathersNameAttr,
        addressLine1 = addressLine1,
        addressLine2 = addressLine2,
        district = districtAttr,
        state = stateAttr,
        pincode = pincodeAttr,
        country = countryAttr,
        photo = photoAttr
      }

parseDLToDLFlow :: (MonadThrow m, Log m) => DLData -> m DigiTypes.DigiLockerDLFlow
parseDLToDLFlow dlData = do
  let classOfVehicles =
        dlData.categories >>= \cats ->
          let abbreviations = Maybe.catMaybes $ map (\cat -> cat.abbreviation) cats
           in if null abbreviations then Nothing else Just abbreviations
  return $
    DigiTypes.DigiLockerDLFlow
      { dlNumber = dlData.dlNumber,
        name = dlData.name,
        dob = dlData.dob,
        dlURL = Nothing, -- DigiLocker XML doesn't provide image URLs
        expiryDate = dlData.expiryDate,
        classOfVehicles = classOfVehicles,
        dateOfIssue = dlData.issueDate
      }

-- | Fetch and extract verified PAN from DigiLocker XML
-- Returns ExtractedDigiLockerPanResp
fetchAndExtractVerifiedPan ::
  ( CoreMetrics m,
    EncFlow m r,
    MonadFlow m
  ) =>
  DigiTypes.DigiLockerCfg ->
  Text ->
  Text ->
  m InterfaceTypes.ExtractedDigiLockerPanResp
fetchAndExtractVerifiedPan config accessToken uri = do
  xmlText <- DigiFlow.getXml config accessToken uri
  doc <- case parseLBS def (TLE.encodeUtf8 $ TL.fromStrict xmlText) of
    Left err -> throwError $ DGLError $ "Failed to parse XML: " <> T.pack (show err)
    Right d -> pure d
  let cursor = fromDocument doc
  panData <- parsePanXML cursor
  let panFlow =
        DigiTypes.DigiLockerPanFlow
          { pan = panData.panNumber,
            name = panData.name,
            dob = panData.dob,
            gender = Nothing, -- Not available in PAN XML
            panURL = Nothing -- DigiLocker XML doesn't provide image URLs
          }
  return $ InterfaceTypes.ExtractedDigiLockerPanResp {extractedPan = Just panFlow}

-- | Fetch and extract verified Aadhaar from DigiLocker XML
-- Returns ExtractedDigiLockerAadhaarResp
fetchAndExtractVerifiedAadhaar ::
  ( CoreMetrics m,
    EncFlow m r,
    MonadFlow m,
    Log m
  ) =>
  DigiTypes.DigiLockerCfg ->
  Text ->
  m InterfaceTypes.ExtractedDigiLockerAadhaarResp
fetchAndExtractVerifiedAadhaar config accessToken = do
  logInfo "Interface.Digilocker.fetchAndExtractVerifiedAadhaar -> invoking Flow.getAadhaarXml"
  xmlText <- DigiFlow.getAadhaarXml config accessToken
  doc <- case parseLBS def (TLE.encodeUtf8 $ TL.fromStrict xmlText) of
    Left err -> throwError $ DGLError $ "Failed to parse XML: " <> T.pack (show err)
    Right d -> pure d
  let cursor = fromDocument doc
  aadhaarData <- parseAadhaarXML cursor
  let -- Combine address components for full address
      addressParts = Maybe.catMaybes [aadhaarData.addressLine1, aadhaarData.addressLine2, aadhaarData.district, aadhaarData.state, aadhaarData.pincode]
      fullAddress = if null addressParts then Nothing else Just $ T.intercalate ", " addressParts
      aadhaarFlow =
        DigiTypes.DigiLockerAadhaarFlow
          { idNumber = aadhaarData.aadhaarNumber,
            fullName = aadhaarData.name,
            dob = aadhaarData.dob,
            address = fullAddress,
            city = aadhaarData.district,
            pincode = aadhaarData.pincode,
            aadhaarFrontURL = Nothing, -- DigiLocker XML doesn't provide image URLs
            aadhaarBackURL = Nothing
          }
  return $ InterfaceTypes.ExtractedDigiLockerAadhaarResp {extractedAadhaar = Just aadhaarFlow}

-- | Get Aadhaar XML from DigiLocker as-is without parsing
-- Takes the DigiLocker config and access token (Bearer token)
-- Returns the raw XML as Text
getVerifiedAadhaarXML ::
  ( CoreMetrics m,
    EncFlow m r,
    MonadFlow m
  ) =>
  DigiTypes.DigiLockerCfg ->
  Text ->
  m Text
getVerifiedAadhaarXML config accessToken = do
  DigiFlow.getAadhaarXml config accessToken
