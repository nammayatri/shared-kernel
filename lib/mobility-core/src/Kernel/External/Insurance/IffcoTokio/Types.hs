module Kernel.External.Insurance.IffcoTokio.Types where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude

data IffcoTokioConfig = IffcoTokioConfig
  { url :: Text,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    masterPolicyClient :: Text,
    insurancePlan :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data HomeDeclarationResp = HomeDeclarationResp
  { certificateNumber :: Text,
    declarationId :: Text,
    status :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
