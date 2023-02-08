module Kernel.External.SMS.ExotelSms.Config where

import Kernel.External.Encryption
import Kernel.External.SMS.ExotelSms.Types
import Kernel.Prelude

data ExotelSmsCfg = ExotelSmsCfg
  { apiKey :: EncryptedField 'AsEncrypted Text,
    apiToken :: EncryptedField 'AsEncrypted Text,
    sid :: ExotelSmsSID,
    url :: ExotelURL
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
