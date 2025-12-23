module Kernel.External.SMS.PinbixSms.Config where

import Kernel.External.Encryption
import Kernel.Prelude

data PinbixSmsCfg = PinbixSmsCfg
  { userId :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    sendMethod :: Text,
    msgType :: Text,
    output :: Text,
    url :: BaseUrl
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
