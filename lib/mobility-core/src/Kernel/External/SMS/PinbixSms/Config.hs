module Kernel.External.SMS.PinbixSms.Config where

import Data.Aeson (defaultOptions)
import Kernel.Prelude

data PinbixSmsCfg = PinbixSmsCfg
  { userId :: Text,
    password :: Text,
    sendMethod :: Text,
    msgType :: Text,
    output :: Text,
    url :: BaseUrl
  }
  deriving (Show, Eq, Generic)

instance FromJSON PinbixSmsCfg where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON PinbixSmsCfg where
  toJSON = genericToJSON defaultOptions
