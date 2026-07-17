module Kernel.External.PartnerSdk.Interface.Types where

import qualified Kernel.External.PartnerSdk.Aarokya.Types as Aarokya
import Kernel.Prelude

data IdProof = IdProof
  { proofType :: Text,
    number :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GenerateTokenReq = GenerateTokenReq
  { phoneCountryCode :: Text,
    phoneNumber :: Text,
    idProof :: IdProof,
    dob :: Maybe Text,
    address :: Maybe Text,
    gender :: Maybe Text,
    firstName :: Maybe Text,
    lastName :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype GenerateTokenResp = GenerateTokenResp
  { accessToken :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A contributor reference: @refType@ is the tag Aarokya expects
-- (@"CUSTOMER_ID"@ or @"PHONE"@) and @refValue@ the corresponding identifier.
data ContributorRef = ContributorRef
  { refType :: Text,
    refValue :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GenerateContributorTokenReq = GenerateContributorTokenReq
  { beneficiaryIdentifier :: Text,
    -- | Aarokya @ContributionChannel@ enum value, e.g. @"CUSTOMER_APP"@.
    contributionChannel :: Maybe Text,
    contributorRef :: Maybe ContributorRef
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype GenerateContributorTokenResp = GenerateContributorTokenResp
  { contributorToken :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PartnerSdkConfig
  = AarokyaPartnerSdkConfig Aarokya.AarokyaSdkConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
