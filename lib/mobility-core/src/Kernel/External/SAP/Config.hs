{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SAP.Config where

import qualified Data.Map.Strict as Map
import Kernel.External.Encryption
import Kernel.Prelude

data SAPAccountConfig = SAPAccountConfig
  { hkont :: Text,
    kostl :: Maybe Text,
    prctr :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SAPServiceConfig = SAPServiceConfig
  { sapAuthUrl :: BaseUrl,
    sapApiUrl :: BaseUrl,
    sapAuthCredentials :: EncryptedField 'AsEncrypted Text,
    bukrs :: Text,
    blart :: Text,
    accountMapping :: Map.Map Text SAPAccountConfig
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
