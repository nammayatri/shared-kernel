{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Moengage.Moengage.Config where

import Kernel.External.Encryption
import Kernel.Prelude

-- | Configuration for Moengage S2S API
data MoengageCfg = MoengageCfg
  { baseUrl :: BaseUrl,
    appId :: Text,
    apiSecret :: EncryptedField 'AsEncrypted Text,
    enabled :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
