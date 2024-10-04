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
    metaData :: Maybe VerifyRCDLAsyncMetaData,
    result :: Maybe VerifyRCDLAsyncResult,
    error :: Maybe Text,
    message :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON VerifyRCAsyncResp where
  parseJSON = withObject "VerifyRCAsyncResp" $ \v -> do
    status <- v .: "status"
    statusCode <- v .: "statusCode"
    metaData <- v .:? "metadata"
    result <- v .:? "result"
    error <- v .:? "error"
    message <- v .:? "message"
    return $ VerifyRCAsyncResp {..}

instance ToJSON VerifyRCAsyncResp where
  toJSON (VerifyRCAsyncResp {..}) =
    object
      [ "status" .= status,
        "statusCode" .= statusCode,
        "metadata" .= metaData,
        "result" .= result,
        "error" .= error,
        "message" .= message
      ]

newtype VerifyRCDLAsyncMetaData = VerifyRCDLAsyncMetaData
  { requestId :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype VerifyRCDLAsyncResult = VerifyRCDLAsyncResult
  { actionStatus :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data HyperVergeDLVerificationReq = HyperVergeDLVerificationReq
  { dlNumber :: Text,
    dob :: Text,
    returnState :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON HyperVergeDLVerificationReq where
  parseJSON = withObject "HyperVergeDLVerificationReq" $ \v -> do
    dlNumber <- v .: "dlNumber"
    dob <- v .: "dob"
    returnState <- (== String "yes") <$> (v .: "returnState")
    return HyperVergeDLVerificationReq {..}

instance ToJSON HyperVergeDLVerificationReq where
  toJSON (HyperVergeDLVerificationReq dlNumber dob returnState) =
    object
      [ "dlNumber" .= dlNumber,
        "dob" .= dob,
        "returnState" .= bool (String "no") (String "yes") returnState
      ]

data HyperVergeDLVerificationResp = HyperVergeDLVerificationResp
  { status :: Text,
    statusCode :: Int,
    metaData :: Maybe VerifyRCDLAsyncMetaData,
    result :: Maybe VerifyRCDLAsyncResult,
    error :: Maybe Text,
    message :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON HyperVergeDLVerificationResp where
  parseJSON = withObject "HyperVergeDLVerificationResp" $ \v -> do
    status <- v .: "status"
    statusCode <- v .: "statusCode"
    metaData <- v .:? "metadata"
    result <- v .:? "result"
    error <- v .:? "error"
    message <- v .:? "message"
    return HyperVergeDLVerificationResp {..}

instance ToJSON HyperVergeDLVerificationResp where
  toJSON HyperVergeDLVerificationResp {..} =
    object
      [ "status" .= status,
        "statusCode" .= statusCode,
        "metadata" .= metaData,
        "result" .= result,
        "error" .= error,
        "message" .= message
      ]

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
    result :: Maybe VerificationResultData,
    error :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data VerificationResultData = RCVerificationResultData RCVerificationData | DLVerificationResultData DLVerificationData
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON VerificationResultData where
  parseJSON v =
    let tryParse :: (FromJSON a) => (a -> VerificationResultData) -> Result VerificationResultData
        tryParse constructor = constructor <$> fromJSON v
     in case tryParse RCVerificationResultData of
          Success result -> return result
          _ -> case tryParse DLVerificationResultData of
            Success result -> return result
            Error str -> fail $ "Could not parse VerificationResultData !!!! Message:" <> str

data RCVerificationData = RCVerificationData
  { transactionId :: Text,
    resultData :: ResData,
    timestamp :: Integer
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON RCVerificationData where
  parseJSON = withObject "RCVerificationData" $ \v -> do
    transactionId <- v .: "transaction_id"
    resultData <- v .: "data"
    timestamp <- v .: "timestamp"
    return $ RCVerificationData {..}

data ResData = ResData
  { code :: Maybe Text,
    message :: Maybe Text,
    rcData :: Maybe RCData
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RCData = RCData
  { taxEndDate :: Maybe Text,
    financed :: Maybe Bool,
    financier :: Maybe Text,
    permitData :: Maybe PermitData,
    documentType :: Maybe Text,
    ownerData :: Maybe OwnerData,
    issueDate :: Maybe Text,
    expiryDate :: Maybe Text,
    registeredAt :: Maybe Text,
    insuranceData :: Maybe InsuranceData,
    vehicleData :: Maybe VehicleData,
    normsType :: Maybe Text,
    nonUseStatus :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON RCData where
  parseJSON = withObject "RCData" $ \v -> do
    taxEndDate <- v .:? "tax_end_date"
    financed <- v .:? "financed"
    financier <- v .:? "financier"
    permitData <- v .:? "permit_data"
    documentType <- v .:? "documentType"
    ownerData <- v .:? "ownerData"
    issueDate <- v .:? "issueDate"
    expiryDate <- v .:? "expiryDate"
    registeredAt <- v .:? "registeredAt"
    insuranceData <- v .:? "insuranceData"
    vehicleData <- v .:? "vehicleData"
    normsType <- v .:? "normsType"
    nonUseStatus <- v .:? "nonUseStatus"
    return $ RCData {..}

instance ToJSON RCData where
  toJSON (RCData {..}) =
    object
      [ "tax_end_date" .= taxEndDate,
        "financed" .= financed,
        "financier" .= financier,
        "permit_data" .= permitData,
        "documentType" .= documentType,
        "ownerData" .= ownerData,
        "issueDate" .= issueDate,
        "expiryDate" .= expiryDate,
        "registeredAt" .= registeredAt,
        "insuranceData" .= insuranceData,
        "vehicleData" .= vehicleData,
        "normsType" .= normsType,
        "nonUseStatus" .= nonUseStatus
      ]

data PermitData = PermitData
  { permitNumber :: Maybe Text,
    permitType :: Maybe Text,
    issueDate :: Maybe Text,
    expiryDate :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON PermitData where
  parseJSON = withObject "PermitData" $ \v -> do
    permitNumber <- v .:? "permit_number"
    permitType <- v .:? "type"
    issueDate <- v .:? "issue_date"
    expiryDate <- v .:? "expiry_date"
    return $ PermitData {..}

instance ToJSON PermitData where
  toJSON (PermitData {..}) =
    object
      [ "permit_number" .= permitNumber,
        "type" .= permitType,
        "issue_date" .= issueDate,
        "expiry_date" .= expiryDate
      ]

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
  deriving (Show, Eq, Generic)

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

instance ToJSON VehicleData where
  toJSON (VehicleData {..}) =
    object
      [ "category" .= category,
        "color" .= color,
        "wheelbase" .= wheelbase,
        "manufactured_Date" .= manufacturedDate,
        "categoryDescription" .= categoryDescription,
        "chassisNumber" .= chassisNumber,
        "engineNumber" .= engineNumber,
        "makerDescription" .= makerDescription,
        "makerModel" .= makerModel,
        "bodyType" .= bodyType,
        "fuelType" .= fuelType,
        "cubicCapacity" .= cubicCapacity,
        "grossWeight" .= grossWeight,
        "numberOfCylinders" .= numberOfCylinders,
        "seatingCapacity" .= seatingCapacity,
        "sleeperCapacity" .= sleeperCapacity,
        "unladenWeight" .= unladenWeight
      ]

data VerificationStatusMetaData = VerificationStatusMetaData
  { requestId :: Maybe Text,
    transactionId :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data DLVerificationData = DLVerificationData
  { issue_date :: Maybe Text,
    father :: Maybe Text,
    name :: Maybe Text,
    img :: Maybe Text,
    blood_group :: Maybe Text,
    dob :: Maybe Text,
    dl_number :: Maybe Text,
    validity :: Maybe DLValidityInfo,
    cov_details :: Maybe [CovDetails],
    address :: Maybe Text,
    state :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON DLVerificationData where
  parseJSON = withObject "DLVerificationData" $ \v -> do
    issue_date <- v .:? "issue_date"
    father <- v .:? "father/husband"
    name <- v .:? "name"
    img <- v .:? "img"
    blood_group <- v .:? "blood_group"
    dob <- v .:? "dob"
    dl_number <- v .:? "dl_number"
    validity <- v .:? "validity"
    cov_details <- v .:? "cov_details"
    address <- v .:? "address"
    state <- v .:? "state"
    return DLVerificationData {..}

instance ToJSON DLVerificationData where
  toJSON (DLVerificationData {..}) =
    object
      [ "issue_date" .= issue_date,
        "father/husband" .= father,
        "name" .= name,
        "img" .= img,
        "blood_group" .= blood_group,
        "dob" .= dob,
        "dl_number" .= dl_number,
        "validity" .= validity,
        "cov_details" .= cov_details,
        "address" .= address,
        "state" .= state
      ]

data DLValidityInfo = DLValidityInfo
  { transport :: Maybe Text,
    nonTransport :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON DLValidityInfo where
  parseJSON = withObject "DLValidityInfo" $ \v -> do
    transport <- v .:? "transport"
    nonTransport <- v .:? "non-transport"
    return DLValidityInfo {..}

instance ToJSON DLValidityInfo where
  toJSON (DLValidityInfo transport nonTransport) =
    object
      [ "transport" .= transport,
        "non-transport" .= nonTransport
      ]

data CovDetails = CovDetails
  { cov :: Text,
    issue_date :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
