{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Verification.Interface.Types
  ( module Kernel.External.Verification.Interface.Types,
  )
where

import Data.Aeson (object, withObject, withText, (.:), (.:?), (.=))
import Data.Time.Calendar (Day)
import Deriving.Aeson
import EulerHS.Prelude
import qualified Kernel.External.Verification.Digilocker.Types as DigiTypes
import qualified Kernel.External.Verification.Ekatra.Types as EkatraTypes
import qualified Kernel.External.Verification.HyperVerge.Types as HyperVergeTypes
import qualified Kernel.External.Verification.Idfy.Config as Idfy
import qualified Kernel.External.Verification.Idfy.Types.Response as Idfy
import qualified Kernel.External.Verification.InternalScripts.Types as FV
import qualified Kernel.External.Verification.Morth.Types as MorthTypes
import qualified Kernel.External.Verification.SafetyPortal.Config as SafetyPortal
import Kernel.External.Verification.SafetyPortal.Types
import qualified Kernel.External.Verification.Tten.Types as TtenTypes
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude

data VerificationServiceConfig = IdfyConfig Idfy.IdfyCfg | FaceVerificationConfig FV.FaceVerificationCfg | GovtDataConfig | HyperVergeVerificationConfig HyperVergeTypes.HyperVergeVerificationCfg | HyperVergeVerificationConfigRCDL HyperVergeTypes.HyperVergeRCDLVerificationConfig | DigiLockerConfig DigiTypes.DigiLockerCfg | TtenVerificationConfig TtenTypes.TtenVerificationCfg | MorthConfig MorthTypes.MorthVerificationCfg | EkatraConfig EkatraTypes.EkatraVerificationCfg | InternalOCRConfig FV.InternalOCRCfg
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype DriverBackgroundVerificationServiceConfig = SafetyPortalConfig SafetyPortal.SafetyPortalCfg
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ImageExtractionHandler m = ImageExtractionHandler
  { getProvidersPriorityList :: m [VT.VerificationService],
    getProviderTimeout :: m Int,
    getProviderConfig :: VT.VerificationService -> m VerificationServiceConfig
  }

data VerifyDLReq = VerifyDLReq
  { dlNumber :: Text,
    driverId :: Text,
    dateOfBirth :: UTCTime,
    returnState :: Maybe Bool,
    -- | Applicant's mobile number (required for MoRTH DL verification)
    applicantMobile :: Maybe Text
  }
  deriving stock (Show, Generic)

data VerifyPanAsyncReq = VerifyPanAsyncReq
  { panNumber :: Text,
    driverId :: Text,
    fullName :: Text,
    dateOfBirth :: UTCTime
  }
  deriving stock (Show, Generic)

data VerifyGstAsyncReq = VerifyGstAsyncReq
  { gstNumber :: Text,
    driverId :: Text,
    filingDetails :: Bool,
    eInvoiceDetails :: Bool
  }
  deriving stock (Show, Generic)

data VerifyBankAccountAsyncReq = VerifyBankAccountAsyncReq
  { bankAccountNo :: Text,
    bankIfscCode :: Text,
    nfVerification :: Bool,
    driverId :: Text
  }
  deriving stock (Show, Generic)

data VerifyPanAadhaarLinkAsyncReq = VerifyPanAadhaarLinkAsyncReq
  { panNumber :: Text,
    aadhaarNumber :: Text,
    driverId :: Text
  }
  deriving stock (Show, Generic)

data VerifyUdyamAadhaarAsyncReq = VerifyUdyamAadhaarAsyncReq
  { uamNumber :: Text,
    driverId :: Text
  }
  deriving stock (Show, Generic)

data VerifyCRCReq = VerifyCRCReq
  { name :: Text,
    fatherName :: Maybe Text,
    dob :: Maybe Day,
    address :: Maybe Text,
    panNumber :: Maybe Text,
    entityType :: VT.CRCEntityType,
    driverId :: Text
  }
  deriving stock (Show, Generic)

data VerifyDLSyncResp = VerifyDLSyncResp
  { requestId :: Maybe Text,
    requestor :: VT.VerificationService,
    transactionId :: Maybe Text,
    response :: DLVerificationOutputInterface
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerifyDLResp = AsyncDLResp VerifyAsyncResp | SyncDLResp VerifyDLSyncResp
  deriving (Show, Generic)

instance ToJSON VerifyDLResp where
  toJSON (AsyncDLResp a) = toJSON a
  toJSON (SyncDLResp s) = toJSON s

instance FromJSON VerifyDLResp where
  parseJSON v = (AsyncDLResp <$> parseJSON v) <|> (SyncDLResp <$> parseJSON v)

type VerifyDLAsyncResp = VerifyDLResp

type VerifyPanAsyncResp = VerifyAsyncResp

type VerifyGstAsyncResp = VerifyAsyncResp

type VerifyBankAccountAsyncResp = VerifyAsyncResp

type VerifyPanAadhaarLinkAsyncResp = VerifyAsyncResp

type VerifyUdyamAadhaarAsyncResp = VerifyAsyncResp

type VerifyCRCAsyncResp = VerifyAsyncResp

data VerifyRCReq = VerifyRCReq
  { rcNumber :: Text,
    driverId :: Text,
    token :: Maybe Text,
    udinNo :: Maybe Text,
    -- | Engine number (required for MoRTH RC verification)
    engineNumber :: Maybe Text,
    -- | Chassis number (required for MoRTH RC verification)
    chassisNumber :: Maybe Text,
    -- | Applicant's mobile number (used by MoRTH RC verification)
    applicantMobile :: Maybe Text
  }
  deriving stock (Show, Generic)

data VerifyRCResp = AsyncResp VerifyAsyncResp | SyncResp VerifySyncResp
  deriving (Show, Generic)

data VerifySyncResp = VerifySyncResp
  { requestId :: Maybe Text,
    requestor :: VT.VerificationService,
    transactionId :: Maybe Text,
    response :: VT.RCVerificationResponse
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance ToJSON VerifyRCResp where
  toJSON (AsyncResp a) = toJSON a
  toJSON (SyncResp s) = toJSON s

instance FromJSON VerifyRCResp where
  parseJSON v = (AsyncResp <$> parseJSON v) <|> (SyncResp <$> parseJSON v)

data VerifyAsyncResp = VerifyAsyncResp
  { requestId :: Text,
    requestor :: VT.VerificationService,
    transactionId :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ValidateImageReq = ValidateImageReq
  { image :: Text,
    imageType :: ImageType,
    driverId :: Text
  }
  deriving stock (Show, Generic)

data ImageType = DriverLicense | VehicleRegistrationCertificate | VehiclePUC | VehiclePermit | VehicleInsurance | VehicleFitnessCertificate | VehicleNOC | PanCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data ValidateImageResp = ValidateImageResp
  { validationAvailable :: Bool,
    detectedImage :: Maybe DetectedImage
  }
  deriving stock (Show, Generic)

data DetectedImage = DetectedImage
  { imageType :: ImageType,
    isReadable :: Maybe Bool,
    confidence :: Maybe Int
  }
  deriving stock (Show, Generic)

type ExtractRCImageReq = ExtractImageReq

type ExtractDLImageReq = ExtractImageReq

type ExtractPanImage = ExtractImageReq

type ExtractGSTImage = ExtractImageReq

data ExtractUdyogAadhaarReq = ExtractUdyogAadhaarReq
  { image1 :: Text,
    driverId :: Text
  }
  deriving stock (Show, Generic)

data ExtractImageReq = ExtractImageReq
  { image1 :: Text,
    image2 :: Maybe Text,
    driverId :: Text
  }
  deriving stock (Show, Generic)

data ExtractAadhaarImageReq = ExtractAadhaarImageReq
  { image1 :: Text,
    image2 :: Maybe Text,
    consent :: Text,
    driverId :: Text
  }

data ExtractedPanImageResp = ExtractedPanImageResp
  { extractedPan :: Maybe Idfy.PanExtractionOutput,
    provider :: Maybe VT.VerificationService
  }
  deriving stock (Show, Generic)

data ExtractedPAN = ExtractedPAN
  { panNumber :: Maybe Text,
    nameOnCard :: Maybe Text,
    dateOfBirth :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype ExtractAadhaarImageRes = ExtractAadhaarImageRes
  { extractedAadhaar :: Maybe Idfy.AadhaarResult
  }

newtype ExtractedGSTImageResp = ExtractedGSTImageResp
  { extractedGST :: Maybe Idfy.GSTExtractionOutput
  }
  deriving stock (Show, Generic)

newtype ExtractedUdyogAadhaarImageResp = ExtractedUdyogAadhaarImageResp
  { extractedUdyogAadhaar :: Maybe Idfy.UdyogAadhaarOutput
  }
  deriving stock (Show, Generic)

data ExtractRCImageResp = ExtractRCImageResp
  { extractedRC :: Maybe ExtractedRC,
    provider :: Maybe VT.VerificationService
  }
  deriving stock (Show, Generic)

data ExtractedRC = ExtractedRC
  { rcNumber :: Maybe Text,
    vehicleClass :: Maybe Text,
    manufacturer :: Maybe Text,
    model :: Maybe Text,
    fuelType :: Maybe Text,
    colour :: Maybe Text,
    chassisNumber :: Maybe Text,
    engineNumber :: Maybe Text,
    registrationDate :: Maybe Text,
    ownerName :: Maybe Text,
    manufacturingDate :: Maybe Text,
    bodyType :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data ExtractDLImageResp = ExtractDLImageResp
  { extractedDL :: Maybe ExtractedDL,
    provider :: Maybe VT.VerificationService
  }
  deriving stock (Show, Generic)

data ExtractedDL = ExtractedDL
  { dlNumber :: Maybe Text,
    nameOnCard :: Maybe Text,
    dateOfBirth :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- not used in interface

data GetTaskReq = GetTaskReq
  { workflowId :: Maybe Text,
    requestId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data GetTaskResp = RCResp VT.RCVerificationResponse | DLResp DLVerificationOutputInterface | PanResp VT.PanVerificationResponse | GstResp VT.GstVerificationResponse | BankAccountResp VT.BankAccountVerificationResponse | PanAadhaarLinkResp VT.PanAadhaarLinkResponse | UdyogAadhaarResp VT.UdyogAadhaarVerificationResponse | UdyamAadhaarResp VT.UdyamAadhaarVerificationResponse | CRCResp VT.CRCVerificationResponse
  deriving (Generic, FromJSON, ToJSON, Show)

data DLVerificationOutputInterface = DLVerificationOutputInterface
  { driverName :: Maybe Text,
    dob :: Maybe Text,
    licenseNumber :: Maybe Text,
    nt_validity_from :: Maybe Text,
    nt_validity_to :: Maybe Text,
    t_validity_from :: Maybe Text,
    t_validity_to :: Maybe Text,
    covs :: Maybe [Idfy.CovDetail],
    status :: Maybe Text,
    dateOfIssue :: Maybe Text,
    message :: Maybe Text
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

data SearchAgentReq = SearchAgentreq
  { dl :: Maybe Text,
    voterId :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype SearchAgentResponse = SearchAgentResponse
  { suspect :: [SearchAgent]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype VerifySdkDataReq = VerifySdkDataReq
  { transactionId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data VerifySdkDataResp = VerifySdkDataResp
  { userDetails :: Maybe HyperVergeTypes.UserDetails,
    status :: Maybe Text,
    transactionId :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data RCRespWithRemPriorityList = RCRespWithRemPriorityList
  { verifyRCResp :: VerifyRCResp,
    remPriorityList :: [VT.VerificationService]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data NameCompareReq = NameCompareReq
  { extractedName :: Text,
    verifiedName :: Text,
    percentage :: Maybe Bool,
    driverId :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype NameCompareResp = NameCompareResp
  { nameComparedData :: Maybe Idfy.NameCompareResponseData
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data FaceCompareReq = FaceCompareReq
  { documentImage1 :: Text,
    documentImage2 :: Text,
    driverId :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype FaceCompareResp = FaceCompareResp
  { faceComparedData :: Maybe Idfy.FaceCompareResponseData
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype ExtractedDigiLockerDLResp = ExtractedDigiLockerDLResp
  { extractedDL :: Maybe DigiTypes.DigiLockerDLFlow
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype ExtractedDigiLockerPanResp = ExtractedDigiLockerPanResp
  { extractedPan :: Maybe DigiTypes.DigiLockerPanFlow
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype ExtractedDigiLockerAadhaarResp = ExtractedDigiLockerAadhaarResp
  { extractedAadhaar :: Maybe DigiTypes.DigiLockerAadhaarFlow
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data VerifyTtenReq = VerifyTtenReq
  { ttenCertificateNumber :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- Internal OCR service types

data OCRRequest = OCRRequest
  { image :: Text,
    imageType :: ImageType,
    driverId :: Text,
    prompt :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype OCRAccepted = OCRAccepted
  { sessionId :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OCRStatus = OCRProcessing | OCRDone | OCRError
  deriving stock (Show, Eq, Generic)

instance FromJSON OCRStatus where
  parseJSON = withText "OCRStatus" $ \case
    "processing" -> pure OCRProcessing
    "done" -> pure OCRDone
    "error" -> pure OCRError
    other -> fail $ "Unknown OCR status: " <> show other

instance ToJSON OCRStatus where
  toJSON = \case
    OCRProcessing -> toJSON ("processing" :: Text)
    OCRDone -> toJSON ("done" :: Text)
    OCRError -> toJSON ("error" :: Text)

data OCRExtractedData
  = OCRExtractedDL ExtractedDL
  | OCRExtractedRC ExtractedRC
  deriving stock (Show, Generic)

instance ToJSON OCRExtractedData where
  toJSON (OCRExtractedDL dl) = toJSON dl
  toJSON (OCRExtractedRC rc) = toJSON rc

data OCRResult = OCRResult
  { sessionId :: Text,
    driverId :: Text,
    imageType :: ImageType,
    status :: OCRStatus,
    extractedData :: Maybe OCRExtractedData,
    errorMessage :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON OCRResult where
  parseJSON = withObject "OCRResult" $ \o -> do
    sessionId <- o .: "sessionId"
    driverId <- o .: "driverId"
    imageType <- o .: "imageType"
    status <- o .: "status"
    mRaw <- o .:? "extractedData"
    extractedData <- case mRaw of
      Nothing -> pure Nothing
      Just v -> case imageType of
        DriverLicense -> Just . OCRExtractedDL <$> parseJSON v
        VehicleRegistrationCertificate -> Just . OCRExtractedRC <$> parseJSON v
        _ -> pure Nothing
    errorMessage <- o .:? "error"
    return OCRResult {..}

instance ToJSON OCRResult where
  toJSON OCRResult {..} =
    object $
      [ "sessionId" .= sessionId,
        "driverId" .= driverId,
        "imageType" .= imageType,
        "status" .= status,
        "error" .= errorMessage
      ]
        ++ maybe [] (\d -> ["extractedData" .= d]) extractedData
