{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Meta.Config where

import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Utils.JSON

-- | Configuration for the Meta WhatsApp Cloud API.
-- The outbound access token is encrypted-at-rest (decrypted only at call time in Flow.hs);
-- its own 'Show' surfaces only the ciphertext, so a derived 'Show' is safe here.
-- Inbound-webhook secrets (verify token, app secret) are app-level env config, not
-- per-merchant, so they live in the consuming service rather than in this stored config.
data MetaCfg = MetaCfg
  { accessToken :: EncryptedField 'AsEncrypted Text,
    baseUrl :: BaseUrl, -- https://graph.facebook.com
    phoneNumberId :: Text,
    apiVersion :: Text -- e.g. "v23.0" — configurable, never hard-coded
  }
  deriving (Eq, Show, Generic)

instance FromJSON MetaCfg where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON MetaCfg where
  toJSON = genericToJSON constructorsWithSnakeCase
