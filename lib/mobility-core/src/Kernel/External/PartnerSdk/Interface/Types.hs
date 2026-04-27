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
    idProof :: IdProof
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype GenerateTokenResp = GenerateTokenResp
  { accessToken :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PartnerSdkConfig
  = AarokyaPartnerSdkConfig Aarokya.AarokyaSdkConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
