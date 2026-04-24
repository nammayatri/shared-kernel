module Kernel.External.PartnerSdk.Aarokya.Types where

import Kernel.External.Encryption
import Kernel.Prelude

data AarokyaTokenRequest = AarokyaTokenRequest
  { phone_country_code :: Text,
    phone_number :: Text,
    platform_id :: Text,
    dl_number :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype AarokyaTokenResponse = AarokyaTokenResponse
  { access_token :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AarokyaSdkConfig = AarokyaSdkConfig
  { url :: BaseUrl,
    apiKey :: EncryptedField 'AsEncrypted Text,
    platformId :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
