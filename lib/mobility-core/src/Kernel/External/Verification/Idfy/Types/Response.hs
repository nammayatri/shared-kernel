{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Verification.Idfy.Types.Response where

import Data.Aeson hiding (Error)
import qualified Data.Aeson as A
import Data.OpenApi hiding (name)
import EulerHS.Prelude hiding (state)
import Kernel.Types.App ()
import Kernel.Utils.JSON
import Kernel.Utils.Time

type ImageValidateResponse = IdfyResponse ValidateResponse

type RCExtractResponse = IdfyResponse (ExtractionOutput RCExtractionOutput)

type DLExtractResponse = IdfyResponse (ExtractionOutput DLExtractionOutput)

type PanExtractionResponse = IdfyResponse (ExtractionOutput PanExtractionOutput)

type GSTExtractionResponse = IdfyResponse (ExtractionOutput GSTExtractionOutput)

type AadhaarExtractionResponse = IdfyResponse AadhaarResult

type VerificationResponse = IdfyResponse IdfyResult

type VerificationResponseList = [IdfyResponse IdfyResult]

type IdfyResult = Output DLVerificationOutput RCVerificationOutput

type NameCompareResponse = IdfyResponse NameCompareResponseData

data IdfyResponse a = IdfyResponse
  { action :: Text,
    completed_at :: UTCTime,
    created_at :: UTCTime,
    group_id :: Text,
    request_id :: Text,
    result :: Maybe a,
    status :: Text,
    task_id :: Text,
    _type :: Text
  }
  deriving (Show, Generic)

instance (ToSchema a) => ToSchema (IdfyResponse a) where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance (FromJSON a) => FromJSON (IdfyResponse a) where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance (ToJSON a) => ToJSON (IdfyResponse a) where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data IdfySuccess = IdfySuccess {request_id :: Text, _a :: Maybe Text}
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- RC Result
newtype ExtractionOutput a = ExtractionOutput {extraction_output :: a}
  deriving (Show, Generic)

instance (ToSchema a) => ToSchema (ExtractionOutput a)

instance (ToJSON a) => ToJSON (ExtractionOutput a)

instance (FromJSON a) => FromJSON (ExtractionOutput a)

-- DL Result
newtype SourceOutput a = SourceOutput {source_output :: a}
  deriving (Show, Generic)

instance (ToSchema a) => ToSchema (SourceOutput a)

instance (ToJSON a) => ToJSON (SourceOutput a)

instance (FromJSON a) => FromJSON (SourceOutput a)

data Output a b = Output {source_output :: Maybe a, extraction_output :: Maybe b}
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- RC verification response
data RCVerificationOutput = RCVerificationOutput
  { avg_gross_vehicle_weight :: Maybe Text,
    axle_configuration :: Maybe Text,
    chassis_number :: Maybe Text,
    emission_norms :: Maybe Text,
    color :: Maybe Text,
    colour :: Maybe Text,
    engine_number :: Maybe Text,
    fitness_upto :: Maybe Text,
    fuel_type :: Maybe Text,
    insurance_details :: Maybe Text,
    insurance_validity :: Maybe Text,
    manufacturer :: Maybe Text,
    mv_tax_upto :: Maybe Text,
    owner_name :: Maybe Text,
    permit_type :: Maybe Text,
    permit_validity_upto :: Maybe Text,
    permit_issue_date :: Maybe Text,
    permit_number :: Maybe Text,
    puc_validity_upto :: Maybe Text,
    registration_date :: Maybe Text,
    registration_number :: Maybe Text,
    rto_name :: Maybe Text,
    status :: Maybe Text,
    vehicle_class :: Maybe Text,
    vehicle_financier :: Maybe Text,
    noc_valid_upto :: Maybe Text,
    seating_capacity :: Maybe A.Value,
    variant :: Maybe Text,
    npermit_upto :: Maybe Text,
    manufacturer_model :: Maybe Text,
    standing_capacity :: Maybe A.Value,
    status_message :: Maybe Text,
    number_of_cylinder :: Maybe A.Value,
    puc_valid_upto :: Maybe Text,
    permanent_address :: Maybe Text,
    permit_no :: Maybe A.Value,
    father_name :: Maybe Text,
    status_verfy_date :: Maybe Text,
    m_y_manufacturing :: Maybe Text,
    gross_vehicle_weight :: Maybe A.Value,
    registered_place :: Maybe Text,
    insurance_policy_no :: Maybe A.Value,
    noc_details :: Maybe Text,
    npermit_issued_by :: Maybe Text,
    sleeper_capacity :: Maybe A.Value,
    current_address :: Maybe Text,
    status_verification :: Maybe Text,
    permit_validity_from :: Maybe Text,
    puc_number :: Maybe A.Value,
    owner_mobile_no :: Maybe A.Value,
    blacklist_status :: Maybe Text,
    body_type :: Maybe Text,
    unladden_weight :: Maybe A.Value,
    insurance_name :: Maybe Text,
    owner_serial_number :: Maybe A.Value,
    vehicle_category :: Maybe Text,
    npermit_no :: Maybe A.Value,
    cubic_capacity :: Maybe A.Value,
    norms_type :: Maybe Text,
    financer :: Maybe Text,
    wheelbase :: Maybe A.Value
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- DL Verification response
data DLVerificationOutput = DLVerificationOutput
  { address :: Maybe Text,
    badge_details :: Maybe Text,
    card_serial_no :: Maybe Text,
    city :: Maybe Text,
    date_of_issue :: Maybe Text,
    date_of_last_transaction :: Maybe Text,
    dl_status :: Maybe Text,
    dob :: Maybe Text,
    face_image :: Maybe Text,
    gender :: Maybe Text,
    hazardous_valid_till :: Maybe Text,
    hill_valid_till :: Maybe Text,
    id_number :: Maybe Text,
    issuing_rto_name :: Maybe Text,
    last_transacted_at :: Maybe Text,
    name :: Maybe Text,
    nt_validity_from :: Maybe Text,
    nt_validity_to :: Maybe Text,
    relatives_name :: Maybe Text,
    source :: Maybe Text,
    status :: Maybe Text,
    t_validity_from :: Maybe Text,
    t_validity_to :: Maybe Text,
    cov_details :: Maybe [CovDetail]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data CovDetail = CovDetail
  { category :: Maybe Text,
    cov :: Text,
    issue_date :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- validate image
data ValidateResponse = ValidateResponse
  { detected_doc_type :: Text,
    is_readable :: Maybe Bool,
    readability :: ReadabilityBody
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data ReadabilityBody = ReadabilityBody
  { confidence :: Maybe Int,
    dummyField :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- DL extract response
data DLExtractionOutput = DLExtractionOutput
  { id_number :: Maybe Text,
    name_on_card :: Maybe Text,
    fathers_name :: Maybe Text,
    date_of_birth :: Maybe Text,
    date_of_validity :: Maybe Text,
    address :: Maybe Text,
    district :: Maybe Text,
    street_address :: Maybe Text,
    pincode :: Maybe Text,
    state :: Maybe Text,
    issue_dates :: Maybe ValidateIssueDate,
    _type :: [Text],
    validity :: Maybe Validity,
    status :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema DLExtractionOutput where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON DLExtractionOutput where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON DLExtractionOutput where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Validity = Validity
  { nt :: Maybe Text,
    t :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema Validity where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions constructorsToUpperOptions

instance FromJSON Validity where
  parseJSON = genericParseJSON constructorsToUpperOptions

instance ToJSON Validity where
  toJSON = genericToJSON constructorsToUpperOptions

data ValidateIssueDate = ValidateIssueDate
  { lmv :: Maybe Text,
    mcwg :: Maybe Text,
    trans :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema ValidateIssueDate where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions constructorsToUpperOptions

instance FromJSON ValidateIssueDate where
  parseJSON = genericParseJSON constructorsToUpperOptions

instance ToJSON ValidateIssueDate where
  toJSON = genericToJSON constructorsToUpperOptions

-- RC Extraction
data RCExtractionOutput = RCExtractionOutput
  { address :: Maybe Text,
    body :: Maybe Text,
    chassis_number :: Maybe Text,
    _class :: Maybe Text,
    colour :: Maybe Text,
    cubic_capacity :: Maybe Text,
    document1_side :: Maybe Text,
    document2_side :: Maybe Text,
    engine_number :: Maybe Text,
    fathers_name :: Maybe Text,
    fuel :: Maybe Text,
    manufacturer :: Maybe Text,
    manufacturing_date :: Maybe Text,
    model :: Maybe Text,
    owner_name :: Maybe Text,
    registration_date :: Maybe Text,
    registration_number :: Maybe Text,
    rto_district :: Maybe Text,
    state :: Maybe Text,
    wheel_base :: Maybe Text,
    status :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema RCExtractionOutput where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON RCExtractionOutput where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON RCExtractionOutput where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data PanExtractionOutput = PanExtractionOutput
  { age :: Maybe Int,
    date_of_birth :: Maybe Text,
    date_of_issue :: Maybe Text,
    fathers_name :: Maybe Text,
    id_number :: Maybe Text,
    is_scanned :: Maybe Bool,
    minor :: Maybe Bool,
    name_on_card :: Maybe Text,
    pan_type :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema PanExtractionOutput where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON PanExtractionOutput where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON PanExtractionOutput where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data GSTExtractionOutput = GSTExtractionOutput
  { address :: Maybe Text,
    constitution_of_business :: Maybe Text,
    date_of_liability :: Maybe Text,
    gstin :: Maybe Text,
    is_provisional :: Maybe Bool,
    legal_name :: Maybe Text,
    pan_number :: Maybe Text,
    trade_name :: Maybe Text,
    type_of_registration :: Maybe Text,
    valid_from :: Maybe Text,
    valid_upto :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema GSTExtractionOutput where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON GSTExtractionOutput where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON GSTExtractionOutput where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data AadhaarExtractionOutput = AadhaarExtractionOutput
  { address :: Maybe Text,
    date_of_birth :: Maybe Text,
    district :: Maybe Text,
    fathers_name :: Maybe Text,
    gender :: Maybe Text,
    house_number :: Maybe Text,
    id_number :: Maybe Text,
    is_scanned :: Maybe Bool,
    name_on_card :: Maybe Text,
    pincode :: Maybe Text,
    state :: Maybe Text,
    street_address :: Maybe Text,
    year_of_birth :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema AadhaarExtractionOutput where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON AadhaarExtractionOutput where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON AadhaarExtractionOutput where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data AadhaarQROutput = AadhaarQROutput
  { address :: Maybe Text,
    date_of_birth :: Maybe Text,
    district :: Maybe Text,
    gender :: Maybe Text,
    house_number :: Maybe Text,
    id_number :: Maybe Text,
    name_on_card :: Maybe Text,
    pincode :: Maybe Text,
    state :: Maybe Text,
    street_address :: Maybe Text,
    year_of_birth :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema AadhaarQROutput where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON AadhaarQROutput where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON AadhaarQROutput where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data AadhaarResult = AadhaarResult
  { extraction_output :: AadhaarExtractionOutput,
    qr_output :: AadhaarQROutput
  }
  deriving (Show, Generic)

instance ToSchema AadhaarResult where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON AadhaarResult where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON AadhaarResult where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype NameMatchOutput = NameMatchOutput
  { name_match :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

newtype NameCompareResponseData = NameCompareResponseData
  { match_output :: NameMatchOutput
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
