module Kernel.External.Maps.Google.Config where

import Kernel.External.Encryption
import Kernel.Prelude

data GoogleCfg = GoogleCfg
  { googleMapsUrl :: BaseUrl,
    googleRoadsUrl :: BaseUrl,
    googleKey :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
