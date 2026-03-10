{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.Sources.Email
  ( EmailConfig (..),
    fetchSettlementFile,
  )
where

import qualified Data.ByteString.Lazy as LBS
import Kernel.External.Encryption
import Kernel.Prelude

data EmailConfig = EmailConfig
  { imapHost :: Text,
    imapPort :: Int,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    folderName :: Text,
    subjectFilter :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Placeholder: Fetch a settlement CSV attachment from an email inbox.
-- TODO: Implement actual IMAP connection and attachment extraction.
fetchSettlementFile ::
  (MonadIO m) =>
  EmailConfig ->
  m (Either Text LBS.ByteString)
fetchSettlementFile _config = do
  pure $ Left "Email source not yet implemented"
