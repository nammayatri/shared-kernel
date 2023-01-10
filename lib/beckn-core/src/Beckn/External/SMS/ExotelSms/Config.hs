module Beckn.External.SMS.ExotelSms.Config where

import Beckn.Prelude
import Beckn.External.Encryption
import Beckn.External.SMS.ExotelSms.Types


data ExotelSmsConfig = ExotelSmsConfig
  { 
    apiKey :: EncryptedField 'AsEncrypted Text,
    apiToken :: EncryptedField 'AsEncrypted Text,
    sid :: ExotelSmsSID,
    url :: ExotelURL
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)