{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SOS.ERSS.Config where

import Kernel.External.Encryption
import Kernel.Prelude

-- | Configuration for C-DAC ERSS (Emergency Response Support System)
data ERSSCfg = ERSSCfg
  { -- | ERSS base URL for both auth and API (e.g., https://lbn.erss.in)
    baseUrl :: BaseUrl,
    -- | OAuth client ID
    clientId :: Text,
    -- | OAuth client secret (encrypted)
    clientSecret :: EncryptedField 'AsEncrypted Text,
    -- | ERSS username (encrypted)
    username :: EncryptedField 'AsEncrypted Text,
    -- | ERSS password (encrypted)
    password :: EncryptedField 'AsEncrypted Text,
    -- | Source identifier for ERSS
    authId :: Text,
    -- | Source verification code
    authCode :: Text,
    -- | State code assigned by C-DAC (e.g. "07" for Delhi)
    stateCode :: Maybe Text,
    -- | Redis key prefix for token caching
    tokenKeyPrefix :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
