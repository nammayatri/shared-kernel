{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Tokenize.HyperVerge.Types where

import Kernel.External.Encryption
import Kernel.Prelude hiding (error)

data HyperVergeTokenizeConfig = HyperVergeTokenizeConfig
  { url :: BaseUrl,
    appId :: Text,
    appKey :: EncryptedField 'AsEncrypted Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data HyperVergeTokenizeRequest = HyperVergeTokenizeRequest
  { appId :: Text,
    appKey :: Text,
    expiry :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype ResultData = ResultData
  { token :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data HyperVergeTokenizeResponse = HyperVergeTokenizeResponse
  { status :: Maybe Text,
    statusCode :: Maybe Text,
    result :: Maybe ResultData,
    error :: Maybe Text,
    message :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
