module Beckn.External.SMS.MyValueFirst.Config where

import Beckn.External.Encryption
import Beckn.Prelude

data MyValueFirstCfg = MyValueFirstCfg
  { username :: EncryptedField 'AsEncrypted Text,
    password :: EncryptedField 'AsEncrypted Text,
    url :: BaseUrl
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
