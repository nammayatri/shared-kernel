module Beckn.External.Maps.MMI.Config where

import Beckn.External.Encryption
import Beckn.Prelude

-- | MMI Service config
data MMICfg = MMICfg
  { mmiAuthUrl :: BaseUrl,
    mmiAuthId :: Text,
    mmiAuthSecret :: EncryptedField 'AsEncrypted Text,
    mmiApiKey :: EncryptedField 'AsEncrypted Text,
    mmiKeyUrl :: BaseUrl,
    mmiNonKeyUrl :: BaseUrl
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
