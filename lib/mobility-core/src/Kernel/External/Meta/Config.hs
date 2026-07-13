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
import qualified Text.Show as TShow

-- | Configuration for the Meta WhatsApp Cloud API.
-- The outbound access token is encrypted-at-rest (decrypted only at call time in Flow.hs).
-- verifyToken / appSecret are consumed by the inbound-webhook layer (later phase).
data MetaCfg = MetaCfg
  { accessToken :: EncryptedField 'AsEncrypted Text,
    baseUrl :: BaseUrl, -- https://graph.facebook.com
    phoneNumberId :: Text,
    apiVersion :: Text, -- e.g. "v23.0" — configurable, never hard-coded
    verifyToken :: Maybe Text, -- webhook GET handshake (consumed by app later)
    appSecret :: Maybe Text -- webhook HMAC secret (consumed by app later)
  }
  deriving (Eq, Generic)

-- Custom Show that REDACTS the plaintext webhook secrets (verifyToken,
-- appSecret) so they can never leak into logs/traces. accessToken is an
-- EncryptedField whose own Show already shows only the ciphertext.
instance Show MetaCfg where
  show MetaCfg {..} =
    "MetaCfg {accessToken = " <> TShow.show accessToken
      <> ", baseUrl = "
      <> TShow.show baseUrl
      <> ", phoneNumberId = "
      <> TShow.show phoneNumberId
      <> ", apiVersion = "
      <> TShow.show apiVersion
      <> ", verifyToken = "
      <> redactMaybe verifyToken
      <> ", appSecret = "
      <> redactMaybe appSecret
      <> "}"
    where
      redactMaybe :: Maybe Text -> String
      redactMaybe Nothing = "Nothing"
      redactMaybe (Just _) = "Just \"<redacted>\""

instance FromJSON MetaCfg where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON MetaCfg where
  toJSON = genericToJSON constructorsWithSnakeCase
