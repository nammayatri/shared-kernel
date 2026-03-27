{-# LANGUAGE DerivingStrategies #-}

{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.Types
  ( SettlementService (..),
    SettlementSource (..),
    ReportType (..),
    SettlementServiceConfig (..),
    SFTPConfig (..),
    EmailConfig (..),
    SettlementSourceConfig (..),
  )
where

import Data.Aeson
import Kernel.External.Encryption (EncKind (..), EncryptedField)
import Kernel.Prelude

data SettlementService = HyperPG | BillDesk | YesBiz
  deriving stock (Show, Read, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

data SettlementSource = SFTP | EMAIL | LOCAL_FILE
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ReportType = PAYMENT | PAYOUT
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Per-provider service configuration (wraps SettlementSourceConfig)
-- ---------------------------------------------------------------------------

data SettlementServiceConfig
  = HyperPGConfig SettlementSourceConfig
  | BillDeskConfig SettlementSourceConfig
  | YesBizConfig SettlementSourceConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Source configuration (SFTP / Email)
-- ---------------------------------------------------------------------------

data SFTPConfig = SFTPConfig
  { host :: Text,
    port :: Int,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    remotePath :: Text,
    privateKeyPath :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data EmailConfig = EmailConfig
  { imapHost :: Text,
    imapPort :: Int,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    folderName :: Text,
    subjectFilter :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data SettlementSourceConfig
  = SFTPSourceConfig SFTPConfig Text
  | EmailSourceConfig EmailConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
