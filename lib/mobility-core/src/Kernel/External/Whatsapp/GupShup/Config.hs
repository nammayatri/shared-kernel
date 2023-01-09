module Kernel.External.Whatsapp.GupShup.Config where

import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Utils.JSON

data GupShupCfg = GupShupCfg
  { userid :: EncryptedField 'AsEncrypted Text,
    password :: EncryptedField 'AsEncrypted Text,
    authScheme :: Text,
    channel :: Text,
    v :: Text,
    url :: Text,
    format :: Text,
    otpCfg :: GupShupOTPCfg
  }
  deriving (Show, Eq, Generic)

instance FromJSON GupShupCfg where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON GupShupCfg where
  toJSON = genericToJSON constructorsWithSnakeCase

data GupShupOTPCfg = GupShupOTPCfg
  { msgType :: Text,
    method :: Text,
    templateId :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON GupShupOTPCfg where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON GupShupOTPCfg where
  toJSON = genericToJSON constructorsWithSnakeCase