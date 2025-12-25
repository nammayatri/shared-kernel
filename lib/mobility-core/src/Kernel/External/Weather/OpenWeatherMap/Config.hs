{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Weather.OpenWeatherMap.Config where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude

data OpenWeatherCfg = OpenWeatherCfg
  { apiKey :: EncryptedField 'AsEncrypted Text,
    url :: BaseUrl
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
