module Kernel.External.SMS.MyValueFirst.Config where

import Kernel.External.Encryption
import Kernel.Prelude

data MyValueFirstCfg = MyValueFirstCfg
  { username :: EncryptedField 'AsEncrypted Text,
    password :: EncryptedField 'AsEncrypted Text,
    url :: BaseUrl
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
