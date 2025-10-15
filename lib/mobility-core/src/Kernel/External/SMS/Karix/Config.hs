module Kernel.External.SMS.Karix.Config where

import Kernel.External.Encryption
import Kernel.Prelude

data KarixCfg = KarixCfg
  { accessKey :: EncryptedField 'AsEncrypted Text,
    url :: BaseUrl
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
