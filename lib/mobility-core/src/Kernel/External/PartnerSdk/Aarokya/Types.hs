module Kernel.External.PartnerSdk.Aarokya.Types where

import qualified Data.Aeson as A
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

-- | Aarokya's @contributor_ref@ is an internally-tagged object
-- (@{ "type": "CUSTOMER_ID" | "PHONE", "value": "..." }@), so it is modelled as
-- a record with a hand-written codec that emits the @type@ key (a Haskell
-- keyword) and drops the trailing underscore.
data AarokyaContributorReference = AarokyaContributorReference
  { type_ :: Text,
    value :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON AarokyaContributorReference where
  toJSON ref = A.object ["type" A..= ref.type_, "value" A..= ref.value]

instance FromJSON AarokyaContributorReference where
  parseJSON = A.withObject "AarokyaContributorReference" $ \o ->
    AarokyaContributorReference <$> o A..: "type" <*> o A..: "value"

data AarokyaContributorTokenRequest = AarokyaContributorTokenRequest
  { beneficiary_identifier :: Text,
    contribution_channel :: Maybe Text,
    contributor_ref :: Maybe AarokyaContributorReference
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype AarokyaContributorTokenResponse = AarokyaContributorTokenResponse
  { contributor_token :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AarokyaSdkConfig = AarokyaSdkConfig
  { url :: BaseUrl,
    basicToken :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
