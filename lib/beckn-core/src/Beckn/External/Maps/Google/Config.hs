module Beckn.External.Maps.Google.Config where

import Beckn.External.Encryption
import Beckn.Prelude

data GoogleCfg = GoogleCfg
  { googleMapsUrl :: BaseUrl,
    googleRoadsUrl :: BaseUrl,
    googleKey :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
