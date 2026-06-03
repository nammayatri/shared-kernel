module Kernel.External.ChallanSearch.Signzy.Config where

import Kernel.External.Encryption
import Kernel.Prelude

data SignzyChallanSearchCfg = SignzyChallanSearchCfg
  { url :: BaseUrl,
    apiKey :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
