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

import Deriving.Aeson
import EulerHS.Prelude
import qualified Kernel.External.Verification.HyperVerge.Types as HyperVergeTypes
import qualified Kernel.External.Verification.Idfy.Config as Idfy
import qualified Kernel.External.Verification.Idfy.Types.Response as Idfy
import qualified Kernel.External.Verification.InternalScripts.Types as FV
import qualified Kernel.External.Verification.SafetyPortal.Config as SafetyPortal
import Kernel.External.Verification.SafetyPortal.Types
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude

data VerificationServiceConfig = IdfyConfig Idfy.IdfyCfg | FaceVerificationConfig FV.FaceVerificationCfg | GovtDataConfig | HyperVergeVerificationConfig HyperVergeTypes.HyperVergeVerificationCfg | HyperVergeVerificationConfigRCDL HyperVergeTypes.HyperVergeRCDLVerificationConfig
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype DriverBackgroundVerificationServiceConfig = SafetyPortalConfig SafetyPortal.SafetyPortalCfg
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data VerifyDLAsyncReq = VerifyDLAsyncReq
  { dlNumber :: Text,
    driverId :: Text,
    dateOfBirth :: UTCTime,
    returnState :: Maybe Bool
  }
  deriving stock (Show, Generic)

type VerifyDLAsyncResp = VerifyAsyncResp

data VerifyRCReq = VerifyRCReq
  { rcNumber :: Text,
    driverId :: Text
  }
  deriving stock (Show, Generic)

data VerifyRCResp = AsyncResp VerifyAsyncResp | SyncResp VT.RCVerificationResponse
  deriving (Show, Generic)

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

data ImageType = DriverLicense | VehicleRegistrationCertificate | VehiclePUC | VehiclePermit | VehicleInsurance | VehicleFitnessCertificate
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

data ExtractImageReq = ExtractImageReq
  { image1 :: Text,
    image2 :: Maybe Text,
    driverId :: Text
  }
  deriving stock (Show, Generic)

newtype ExtractRCImageResp = ExtractRCImageResp
  { extractedRC :: Maybe ExtractedRC
  }
  deriving stock (Show, Generic)

newtype ExtractedRC = ExtractedRC
  { rcNumber :: Maybe Text
  }
  deriving stock (Show, Generic)

newtype ExtractDLImageResp = ExtractDLImageResp
  { extractedDL :: Maybe ExtractedDL
  }
  deriving stock (Show, Generic)

data ExtractedDL = ExtractedDL
  { dlNumber :: Maybe Text,
    nameOnCard :: Maybe Text
  }
  deriving stock (Show, Generic)

-- not used in interface

data GetTaskReq = GetTaskReq
  { workflowId :: Maybe Text,
    requestId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data GetTaskResp = RCResp VT.RCVerificationResponse | DLResp DLVerificationOutputInterface
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
    dateOfIssue :: Maybe Text
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
