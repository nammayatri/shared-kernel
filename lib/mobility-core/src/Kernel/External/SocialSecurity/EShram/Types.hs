module Kernel.External.SocialSecurity.EShram.Types where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Utils.Text (encodeToText)

data EShramConfig = EShramConfig
  { url :: BaseUrl,
    subscriptionKey :: EncryptedField 'AsEncrypted Text,
    userEmail :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    tokenSecretCode :: EncryptedField 'AsEncrypted Text,
    tokenSaltValue :: EncryptedField 'AsEncrypted Text,
    secretCode :: EncryptedField 'AsEncrypted Text,
    saltValue :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data EShramResp = EShramResp
  { code :: Int,
    message :: Maybe Text,
    responseData :: Maybe Value,
    jwtToken :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON EShramResp where
  parseJSON = withObject "EShramResp" $ \v ->
    EShramResp
      <$> v .: "code"
      <*> v .:? "message"
      <*> v .:? "data"
      <*> v .:? "jwtToken"

instance ToJSON EShramResp where
  toJSON EShramResp {..} =
    object
      [ "code" .= code,
        "message" .= message,
        "data" .= responseData,
        "jwtToken" .= jwtToken
      ]

data AuthTokenPayload = AuthTokenPayload
  { email :: Text,
    pwd :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype AuthTokenReq = AuthTokenReq
  { encRequest :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GetUnverifiedPWDataReq = GetUnverifiedPWDataReq
  { fromDate :: Text,
    toDate :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data UnverifiedPWData = UnverifiedPWData
  { totalRecord :: Int,
    recordShared :: Int,
    remainingRecord :: Int,
    pwData :: [PWData]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PWData = PWData
  { uanNo :: Text,
    panNo :: Text,
    yob :: Maybe Text,
    gender :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- verificationStatus: 0 = not processed, 1 = matching, 2 = not matching
data VerifiedPWData = VerifiedPWData
  { panNo :: Text,
    uanNo :: Text,
    verificationStatus :: Int,
    verificationRejectionReason :: Maybe Text,
    dateOfOnboarding :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON)

instance ToJSON VerifiedPWData where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

newtype PushVerifiedPWDataReq = PushVerifiedPWDataReq
  { encVerifiedJsonRequest :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PushVerifiedPWDataResult = PushVerifiedPWDataResult
  { recordsFailed :: RecordBucket,
    recordsUpdated :: RecordBucket,
    recordsNotUpdated :: RecordBucket,
    validationError :: RecordBucket
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data MonthlyUtilization = MonthlyUtilization
  { month :: Int,
    noOfDays :: Int,
    uanNo :: Text,
    year :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype InsertPWMonthlyUtilizationReq = InsertPWMonthlyUtilizationReq
  { encMonthlyUtilizationJsonRequest :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data InsertPWMonthlyUtilizationResult = InsertPWMonthlyUtilizationResult
  { recordsUpdated :: RecordBucket,
    recordsInserted :: RecordBucket,
    recordsNotFound :: RecordBucket,
    validationError :: RecordBucket
  }
  deriving (Show, Eq, Generic, ToJSON)

-- eShram spells the inserted bucket "recordsInsterted"
instance FromJSON InsertPWMonthlyUtilizationResult where
  parseJSON = withObject "InsertPWMonthlyUtilizationResult" $ \v ->
    InsertPWMonthlyUtilizationResult
      <$> v .: "recordsUpdated"
      <*> v .: "recordsInsterted"
      <*> v .: "recordsNotFound"
      <*> v .: "validationError"

data RecordBucket = RecordBucket
  { recordCount :: Int,
    dataList :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON)

-- dataList holds UANs; any non-string item is kept as its JSON text instead of failing the parse
instance FromJSON RecordBucket where
  parseJSON = withObject "RecordBucket" $ \v ->
    RecordBucket
      <$> v .: "recordCount"
      <*> (map valueToText <$> v .: "dataList")
    where
      valueToText (String t) = t
      valueToText other = encodeToText other
