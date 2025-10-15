{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Kernel.External.SMS.Karix.Types where

import Data.Aeson
import EulerHS.Prelude

-- message item used by Karix
data KarixMessage = KarixMessage
  { dest :: [Text],
    text :: Text,
    send :: Text,
    type_ :: Text
  }
  deriving (Generic, Show, Eq)

instance ToJSON KarixMessage where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = \f -> if f == "type_" then "type" else f}

instance FromJSON KarixMessage where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = \f -> if f == "type_" then "type" else f}

-- top-level request
data KarixRequest = KarixRequest
  { ver :: Text,
    key :: Text,
    messages :: [KarixMessage]
  }
  deriving (Generic, Show, Eq, ToJSON)

data KarixStatus = KarixStatus
  { code :: Text,
    desc :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data KarixResponse = KarixResponse
  { status :: KarixStatus,
    time :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

newtype KarixSubmitRes = KarixSubmitRes
  { response :: KarixResponse
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)
