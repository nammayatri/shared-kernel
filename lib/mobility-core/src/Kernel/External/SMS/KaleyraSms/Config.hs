module Kernel.External.SMS.KaleyraSms.Config where

import Kernel.External.Encryption
import Kernel.Prelude

data KaleyraSmsCfg = KaleyraSmsCfg
  { apiKey :: EncryptedField 'AsEncrypted Text,
    sid :: EncryptedField 'AsEncrypted Text,
    url :: BaseUrl
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
