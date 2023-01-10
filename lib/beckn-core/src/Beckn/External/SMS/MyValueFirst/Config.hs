module Beckn.External.SMS.MyValueFirst.Config where

import Beckn.Prelude
import Beckn.External.Encryption

data MyValueFirstConfig = MyValueFirstConfig
  { username :: EncryptedField 'AsEncrypted Text,
    password :: EncryptedField 'AsEncrypted Text,
    url :: BaseUrl
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)