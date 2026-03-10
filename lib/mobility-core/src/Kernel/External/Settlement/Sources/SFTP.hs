{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.Sources.SFTP
  ( SFTPConfig (..),
    fetchSettlementFile,
  )
where

import qualified Data.ByteString.Lazy as LBS
import Kernel.External.Encryption
import Kernel.Prelude

data SFTPConfig = SFTPConfig
  { host :: Text,
    port :: Int,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    remotePath :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Placeholder: Fetch a settlement CSV file from an SFTP server.
-- TODO: Implement actual SFTP connection using libssh2 or similar.
fetchSettlementFile ::
  (MonadIO m) =>
  SFTPConfig ->
  Text ->
  m (Either Text LBS.ByteString)
fetchSettlementFile _config _fileName = do
  pure $ Left "SFTP source not yet implemented"
