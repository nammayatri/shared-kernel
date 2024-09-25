{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Verification.HyperVerge.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude hiding (error)

data HyperVergeVerificationCfg = HyperVergeVerificationCfg
  { url :: BaseUrl,
    appId :: Text,
    appKey :: EncryptedField 'AsEncrypted Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype HyperVergeSdkVerificationReq = HyperVergeSdkVerificationReq
  { transactionId :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype Metadata = Metadata
  { requestId :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Sources = Sources
  { source :: Maybe Text,
    subSource :: Maybe Text,
    values :: Maybe Object
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype SelfieFlow = SelfieFlow
  { selfieURL :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data AadhaarFlow = AadhaarFlow
  { idNumber :: Maybe Text,
    fullName :: Maybe Text,
    dob :: Maybe Text,
    address :: Maybe Text,
    city :: Maybe Text,
    pincode :: Maybe Text,
    aadhaarFrontURL :: Text,
    aadhaarBackURL :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data PanFlow = PanFlow
  { pan :: Maybe Text,
    name :: Maybe Text,
    dob :: Maybe Text,
    gender :: Maybe Text,
    panURL :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data UserDetails = HVSelfieFlow SelfieFlow | HVAadhaarFlow AadhaarFlow | HVPanFlow PanFlow
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON UserDetails where
  parseJSON = withObject "UserDetails" $ \v ->
    (HVAadhaarFlow <$> parseJSON (Object v))
      <|> (HVPanFlow <$> parseJSON (Object v))
      <|> (HVSelfieFlow <$> parseJSON (Object v))

data HVResult = HVResult
  { flags :: Maybe [Sources],
    userDetails :: Maybe UserDetails,
    status :: Maybe Text,
    transactionId :: Maybe Text,
    failureReason :: Maybe Text,
    error :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data HyperVergeSdkVerificationRes = HyperVergeSdkVerificationRes
  { status :: Maybe Text,
    statusCode :: Maybe Int,
    metadata :: Maybe Metadata,
    result :: Maybe HVResult
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data VerifyRCAsyncReq = VerifyRCAsyncReq
  { rc :: Text,
    consent :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON VerifyRCAsyncReq where
  parseJSON = withObject "VerifyRCAsyncReq" $ \v -> do
    rc <- v .: "RC"
    consent <- v .: "consent"
    return $ VerifyRCAsyncReq {..}

instance ToJSON VerifyRCAsyncReq where
  toJSON (VerifyRCAsyncReq rc consent) =
    object
      [ "RC" .= rc,
        "consent" .= consent
      ]

data VerifyRCAsyncResp = VerifyRCAsyncResp
  { status :: Text,
    statusCode :: Int,
    metaData :: Maybe VerifyRCAsyncMetaData,
    result :: Maybe VerifyRCAsyncResult,
    error :: Maybe Text,
    message :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON VerifyRCAsyncResp where
  parseJSON = withObject "VerifyRCAsyncResp" $ \v -> do
    status <- v .: "status"
    statusCode <- v .: "statusCode"
    metaData <- v .:? "metadata"
    result <- v .:? "result"
    error <- v .:? "error"
    message <- v .:? "message"
    return $ VerifyRCAsyncResp {..}

newtype VerifyRCAsyncMetaData = VerifyRCAsyncMetaData
  { requestId :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype VerifyRCAsyncResult = VerifyRCAsyncResult
  { actionStatus :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data GetVerificationStatusResp = GetVerificationStatusResp
  { status :: Text,
    statusCode :: Int,
    result :: Maybe VerificationStatusResult,
    metaData :: Maybe VerificationStatusMetaData,
    message :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data VerificationStatusResult = VerificationStatusResult
  { apiStatusCode :: Int,
    apiOutput :: VerificationAPIResp
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data VerificationAPIResp = VerificationAPIResp
  { status :: Text,
    statusCode :: Text,
    result :: VerificationResultData
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data VerificationResultData = VerificationResultData
  { transactionId :: Text,
    resultData :: ResData,
    timestamp :: Integer
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON VerificationResultData where
  parseJSON = withObject "VerificationResultData" $ \v -> do
    transactionId <- v .: "transaction_id"
    resultData <- v .: "data"
    timestamp <- v .: "timestamp"
    return $ VerificationResultData {..}

data ResData = ResData
  { code :: Maybe Text,
    message :: Maybe Text,
    rcData :: Maybe RCData
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RCData = RCData
  { taxEndDate :: Maybe Text,
    financed :: Maybe Bool,
    permitData :: Maybe PermitData,
    documentType :: Maybe Text,
    ownerData :: Maybe OwnerData,
    issueDate :: Maybe Text,
    expiryDate :: Maybe Text,
    registeredAt :: Maybe Text,
    insuranceData :: Maybe InsuranceData,
    vehicleData :: Maybe VehicleData,
    normsType :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON RCData where
  parseJSON = withObject "RCData" $ \v -> do
    taxEndDate <- v .:? "tax_end_date"
    financed <- v .:? "financed"
    permitData <- v .:? "permit_data"
    documentType <- v .:? "documentType"
    ownerData <- v .:? "ownerData"
    issueDate <- v .:? "issueDate"
    expiryDate <- v .:? "expiryDate"
    registeredAt <- v .:? "registeredAt"
    insuranceData <- v .:? "insauranceData"
    vehicleData <- v .:? "vehicleData"
    normsType <- v .:? "normsType"
    return $ RCData {..}

data PermitData = PermitData
  { permitNumber :: Maybe Text,
    permitType :: Maybe Text,
    expiryDate :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON PermitData where
  parseJSON = withObject "PermitData" $ \v -> do
    permitNumber <- v .:? "permit_number"
    permitType <- v .:? "type"
    expiryDate <- v .:? "expiry_date"
    return $ PermitData {..}

data OwnerData = OwnerData
  { serial :: Maybe Text,
    name :: Maybe Text,
    fatherName :: Maybe Text,
    presentAddress :: Maybe Text,
    permanentAddress :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data InsuranceData = InsuranceData
  { company :: Maybe Text,
    policyNumber :: Maybe Text,
    expiryDate :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data VehicleData = VehicleData
  { category :: Maybe Text,
    color :: Maybe Text,
    wheelbase :: Maybe Text,
    manufacturedDate :: Maybe Text,
    categoryDescription :: Maybe Text,
    chassisNumber :: Maybe Text,
    engineNumber :: Maybe Text,
    makerDescription :: Maybe Text,
    makerModel :: Maybe Text,
    bodyType :: Maybe Text,
    fuelType :: Maybe Text,
    cubicCapacity :: Maybe Text,
    grossWeight :: Maybe Text,
    numberOfCylinders :: Maybe Text,
    seatingCapacity :: Maybe Text,
    sleeperCapacity :: Maybe Text,
    unladenWeight :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON VehicleData where
  parseJSON = withObject "VehicleData" $ \v -> do
    category <- v .:? "category"
    color <- v .:? "color"
    wheelbase <- v .:? "wheelbase"
    manufacturedDate <- v .:? "manufactured_Date"
    categoryDescription <- v .:? "categroyDescription"
    chassisNumber <- v .:? "chassisNumber"
    engineNumber <- v .:? "engineNumber"
    makerDescription <- v .:? "makerDescription"
    makerModel <- v .:? "makerModel"
    bodyType <- v .:? "bodyType"
    fuelType <- v .:? "fuelType"
    cubicCapacity <- v .:? "cubicCapacity"
    grossWeight <- v .:? "grossWeight"
    numberOfCylinders <- v .:? "numberOfCylinders"
    seatingCapacity <- v .:? "seatingCapacity"
    sleeperCapacity <- v .:? "sleeperCapacity"
    unladenWeight <- v .:? "unladenWeight"
    return $ VehicleData {..}

data VerificationStatusMetaData = VerificationStatusMetaData
  { requestId :: Maybe Text,
    transactionId :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
