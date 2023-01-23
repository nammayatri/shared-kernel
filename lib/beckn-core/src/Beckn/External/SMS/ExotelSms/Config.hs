module Beckn.External.SMS.ExotelSms.Config where

import Beckn.External.Encryption
import Beckn.External.SMS.ExotelSms.Types
import Beckn.Prelude

data ExotelSmsCfg = ExotelSmsCfg
  { apiKey :: EncryptedField 'AsEncrypted Text,
    apiToken :: EncryptedField 'AsEncrypted Text,
    sid :: ExotelSmsSID,
    url :: ExotelURL
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
