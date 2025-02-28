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
import Kernel.Prelude hiding (error, length)

data HyperVergeVerificationCfg = HyperVergeVerificationCfg
  { url :: BaseUrl,
    appId :: Text,
    appKey :: EncryptedField 'AsEncrypted Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data HyperVergeRCDLVerificationConfig = HyperVergeRCDLVerificationConfig
  { username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    url :: BaseUrl,
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

newtype VerifyRCAsyncReq = VerifyRCAsyncReq
  { reg_no :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

type VerifyRCAsyncResp = HyperVergeVerificationAsyncResp

data HyperVergeVerificationAsyncResp = HyperVergeVerificationAsyncResp
  { status :: Text,
    statusCode :: Int,
    metaData :: Maybe VerifyRCDLAsyncMetaData,
    result :: Maybe VerifyRCDLAsyncResult,
    error :: Maybe Text,
    message :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON HyperVergeVerificationAsyncResp where
  parseJSON = withObject "HyperVergeVerificationAsyncResp" $ \v -> do
    status <- v .: "status"
    statusCode <- v .: "statusCode"
    metaData <- v .:? "metadata"
    result <- v .:? "result"
    error <- v .:? "error"
    message <- v .:? "message"
    return $ HyperVergeVerificationAsyncResp {..}

instance ToJSON HyperVergeVerificationAsyncResp where
  toJSON (HyperVergeVerificationAsyncResp {..}) =
    object
      [ "status" .= status,
        "statusCode" .= statusCode,
        "metadata" .= metaData,
        "result" .= result,
        "error" .= error,
        "message" .= message
      ]

data VerifyRCDLAsyncMetaData = VerifyRCDLAsyncMetaData
  { transactionId :: Maybe Text,
    requestId :: Maybe Text
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

type HyperVergeDLVerificationResp = HyperVergeVerificationAsyncResp

data GetVerificationStatusResp = GetVerificationStatusResp
  { status :: Text,
    statusCode :: Int,
    result :: Maybe VerificationStatusResult,
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
    metaData :: Maybe VerifyRCDLAsyncMetaData,
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
  { message :: Text,
    response_type :: Maybe Text,
    rcInfo :: Maybe RCInfo
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RCInfo = RCInfo
  { state_code :: Maybe Text,
    state :: Maybe Text,
    office_code :: Maybe Int,
    office_name :: Maybe Text,
    reg_no :: Maybe Text,
    reg_date :: Maybe Text,
    purchase_date :: Maybe Text,
    owner_count :: Maybe Int,
    owner_name :: Maybe Text,
    owner_father_name :: Maybe Text,
    current_address_line1 :: Maybe Text,
    current_address_line2 :: Maybe Text,
    current_address_line3 :: Maybe Text,
    current_district_name :: Maybe Text,
    current_state :: Maybe Text,
    current_state_name :: Maybe Text,
    current_pincode :: Maybe Integer,
    current_full_address :: Maybe Text,
    permanent_address_line1 :: Maybe Text,
    permanent_address_line2 :: Maybe Text,
    permanent_address_line3 :: Maybe Text,
    permanent_district_name :: Maybe Text,
    permanent_state :: Maybe Text,
    permanent_state_name :: Maybe Text,
    permanent_pincode :: Maybe Integer,
    permanent_full_address :: Maybe Text,
    owner_code_descr :: Maybe Text,
    reg_type_descr :: Maybe Text,
    vehicle_class_desc :: Maybe Text,
    chassis_no :: Maybe Text,
    engine_no :: Maybe Text,
    vehicle_manufacturer_name :: Maybe Text,
    model_code :: Maybe Text,
    model :: Maybe Text,
    body_type :: Maybe Text,
    cylinders_no :: Maybe Int,
    vehicle_hp :: Maybe Float,
    vehicle_seat_capacity :: Maybe Int,
    vehicle_standing_capacity :: Maybe Int,
    vehicle_sleeper_capacity :: Maybe Int,
    unladen_weight :: Maybe Float,
    vehicle_gross_weight :: Maybe Float,
    vehicle_gross_comb_weight :: Maybe Int,
    fuel_descr :: Maybe Text,
    color :: Maybe Text,
    manufacturing_mon :: Maybe Int,
    manufacturing_yr :: Maybe Int,
    norms_descr :: Maybe Text,
    wheelbase :: Maybe Int,
    cubic_cap :: Maybe Int,
    floor_area :: Maybe Int,
    ac_fitted :: Maybe Text,
    audio_fitted :: Maybe Text,
    video_fitted :: Maybe Text,
    vehicle_catg :: Maybe Text,
    sale_amount :: Maybe Int,
    length :: Maybe Int,
    width :: Maybe Int,
    height :: Maybe Int,
    reg_upto :: Maybe Text,
    fit_upto :: Maybe Text,
    imported_vehicle :: Maybe Text,
    status :: Maybe Text,
    vehicle_type :: Maybe Text,
    tax_mode :: Maybe Text,
    vehicle_insurance_details :: Maybe InsuranceData,
    vehicle_pucc_details :: Maybe PUCDetails,
    permit_details :: Maybe PermitData,
    latest_tax_details :: Maybe TaxDetails,
    financer_details :: Maybe FinancerDetails
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data PermitData = PermitData
  { appl_no :: Maybe Text,
    pmt_no :: Maybe Text,
    reg_no :: Maybe Text,
    rcpt_no :: Maybe Text,
    purpose :: Maybe Text,
    permit_type :: Maybe Text,
    permit_catg :: Maybe Text,
    permit_issued_on :: Maybe Text,
    permit_valid_from :: Maybe Text,
    permit_valid_upto :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data OwnerData = OwnerData
  { serial :: Maybe Text,
    name :: Maybe Text,
    fatherName :: Maybe Text,
    presentAddress :: Maybe Text,
    permanentAddress :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data InsuranceData = InsuranceData
  { insurance_from :: Maybe Text,
    insurance_upto :: Maybe Text,
    insurance_company_code :: Maybe Int,
    insurance_company_name :: Maybe Text,
    opdt :: Maybe Text,
    policy_no :: Maybe Text,
    reg_no :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data TaxDetails = TaxDetails
  { reg_no :: Maybe Text,
    tax_mode :: Maybe Text,
    payment_mode :: Maybe Text,
    tax_amt :: Maybe Int,
    tax_fine :: Maybe Int,
    rcpt_dt :: Maybe Text,
    tax_from :: Maybe Text,
    tax_upto :: Maybe Text,
    collected_by :: Maybe Text,
    rcpt_no :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data FinancerDetails = FinancerDetails
  { hp_type :: Maybe Text,
    financer_name :: Maybe Text,
    financer_address_line1 :: Maybe Text,
    financer_address_line2 :: Maybe Text,
    financer_address_line3 :: Maybe Text,
    financer_district :: Maybe Int,
    financer_pincode :: Maybe Int,
    financer_state :: Maybe Text,
    financer_full_address :: Maybe Text,
    hypothecation_dt :: Maybe Text,
    op_dt :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data PUCDetails = PUCDetails
  { pucc_from :: Maybe Text,
    pucc_upto :: Maybe Text,
    pucc_centreno :: Maybe Text,
    pucc_no :: Maybe Text,
    op_dt :: Maybe Text
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
