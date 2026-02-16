{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Kernel.External.SMS.KarixSms.Types where

import Data.Aeson
import EulerHS.Prelude

-- message item used by KarixSms
data KarixSmsMessage = KarixSmsMessage
  { dest :: [Text],
    text :: Text,
    send :: Text
  }
  deriving (Generic, Show, Eq)

instance ToJSON KarixSmsMessage where
  toJSON = genericToJSON defaultOptions

instance FromJSON KarixSmsMessage where
  parseJSON = genericParseJSON defaultOptions

-- top-level request
data KarixSmsRequest = KarixSmsRequest
  { ver :: Text,
    key :: Text,
    messages :: [KarixSmsMessage]
  }
  deriving (Generic, Show, Eq, ToJSON)

data KarixSmsStatus = KarixSmsStatus
  { code :: Text,
    desc :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data KarixSmsResponse = KarixSmsResponse
  { ackid :: Maybe Text,
    time :: Maybe Text,
    status :: KarixSmsStatus
  }
  deriving (Generic, Show, Eq)

instance FromJSON KarixSmsResponse where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON KarixSmsResponse where
  toJSON = genericToJSON defaultOptions

newtype KarixSmsSubmitRes = KarixSmsSubmitRes
  { response :: KarixSmsResponse
  }
  deriving (Generic, Show, Eq)

instance FromJSON KarixSmsSubmitRes where
  parseJSON v = KarixSmsSubmitRes <$> parseJSON v

instance ToJSON KarixSmsSubmitRes where
  toJSON (KarixSmsSubmitRes resp) = toJSON resp
