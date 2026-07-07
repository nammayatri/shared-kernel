{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kernel.External.Verification.Ekatra.Types where

import Data.Aeson (Value)
import qualified Data.HashMap.Strict as HMS
import Kernel.External.Encryption
import Kernel.Prelude
import Network.HTTP.Client as Http
import Network.HTTP.Client.TLS as Http

data EkatraVerificationCfg = EkatraVerificationCfg
  { url :: BaseUrl,
    apiKey :: EncryptedField 'AsEncrypted Text,
    dlPrompt :: Text,
    rcPrompt :: Text,
    aadhaarPrompt :: Text,
    complexLayout :: Maybe Bool,
    maskAadhaar :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype EkatraOcrResponse = EkatraOcrResponse {getEkatraOcrValue :: Value}
  deriving stock (Show, Generic)
  deriving newtype (FromJSON, ToJSON)

prepareEkatraHttpManager :: Int -> HMS.HashMap Text Http.ManagerSettings
prepareEkatraHttpManager timeout =
  HMS.singleton ekatraHttpManagerKey $
    Http.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

ekatraHttpManagerKey :: Text
ekatraHttpManagerKey = "ekatra-http-manager"
