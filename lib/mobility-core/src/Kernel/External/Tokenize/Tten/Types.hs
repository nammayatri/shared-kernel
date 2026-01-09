{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Tokenize.Tten.Types where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude

data TtenTokenizationConfig = TtenTokenizationConfig
  { url :: BaseUrl,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data GenerateTokenResp = GenerateTokenResp
  { version :: Maybe Text,
    status :: Maybe Int,
    message :: Maybe Text,
    data_ :: Maybe TokenInfo
  }
  deriving stock (Show, Eq, Generic)

data TokenInfo = TokenInfo
  { access_token :: Text,
    created_at :: Text,
    expires_at :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

jsonOptionsGenerateTokenResp :: Options
jsonOptionsGenerateTokenResp =
  defaultOptions
    { fieldLabelModifier = \case
        "type_" -> "type"
        "data_" -> "data"
        other -> other
    }

instance FromJSON GenerateTokenResp where
  parseJSON = genericParseJSON jsonOptionsGenerateTokenResp

instance ToJSON GenerateTokenResp where
  toJSON = genericToJSON jsonOptionsGenerateTokenResp
