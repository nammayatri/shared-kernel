{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Tokenize.JourneyMonitoring.Types where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude hiding (error)

data JourneyMonitoringTokenizeConfig = JourneyMonitoringTokenizeConfig
  { url :: BaseUrl,
    username :: EncryptedField 'AsEncrypted Text,
    password :: EncryptedField 'AsEncrypted Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TokenInfo = TokenInfo
  { accessToken :: Maybe Text,
    createdAt :: Maybe Text,
    expiresAt :: Maybe Text
  }
  deriving (Show, Eq, Generic)

data JourneyMonitoringTokenizeResponse = JourneyMonitoringTokenizeResponse
  { version :: Maybe Text,
    status :: Maybe Int,
    message :: Maybe Text,
    tokenData :: Maybe TokenInfo
  }
  deriving (Show, Eq, Generic)

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "tokenData" -> "data"
        other -> other
    }

jsonOptionsData :: Options
jsonOptionsData =
  defaultOptions
    { fieldLabelModifier = \case
        "accessToken" -> "access_token"
        "createdAt" -> "created_at"
        "expiresAt" -> "expires_at"
        other -> other
    }

instance FromJSON TokenInfo where
  parseJSON = genericParseJSON jsonOptionsData

instance ToJSON TokenInfo where
  toJSON = genericToJSON jsonOptionsData

instance FromJSON JourneyMonitoringTokenizeResponse where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON JourneyMonitoringTokenizeResponse where
  toJSON = genericToJSON jsonOptions
