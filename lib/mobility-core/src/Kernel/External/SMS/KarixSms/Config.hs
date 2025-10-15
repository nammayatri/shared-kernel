module Kernel.External.SMS.KarixSms.Config where

import Kernel.External.Encryption
import Kernel.Prelude

data KarixSmsCfg = KarixSmsCfg
  { accessKey :: EncryptedField 'AsEncrypted Text,
    url :: BaseUrl
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
