module Kernel.External.PartnerSdk.Aarokya.Types where

import Kernel.External.Encryption
import Kernel.Prelude

data AarokyaIdProof = AarokyaIdProof
  { proof_type :: Text,
    number :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AarokyaTokenRequest = AarokyaTokenRequest
  { phone_country_code :: Text,
    phone_number :: Text,
    id_proof :: AarokyaIdProof,
    dob :: Maybe Text,
    address :: Maybe Text,
    gender :: Maybe Text,
    first_name :: Maybe Text,
    last_name :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype AarokyaTokenResponse = AarokyaTokenResponse
  { access_token :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AarokyaSdkConfig = AarokyaSdkConfig
  { url :: BaseUrl,
    basicToken :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
