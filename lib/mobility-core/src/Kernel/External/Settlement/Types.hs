{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    SplitSettlementCustomerType (..),
    SettlementParserTypeMap (..),
    SettlementServiceConfig (..),
    SFTPConfig (..),
    EmailConfig (..),
    JuspayApiConfig (..),
    SettlementSourceConfig (..),
    JuspayOrderStatusConfig (..),
  )
where

import Data.Aeson (FromJSONKey, ToJSONKey)
import qualified Data.Map.Strict as Map
import Kernel.External.Encryption (EncKind (..), EncryptedField)
import Kernel.Prelude

data SettlementService = HyperPG | BillDesk | YesBiz
  deriving stock (Show, Read, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

data SettlementSource = SFTP | EMAIL | API | LOCAL_FILE
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ReportType = PAYMENT | PAYOUT
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SplitSettlementCustomerType = VENDOR | MERCHANT
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | Maps each 'SplitSettlementCustomerType' to file basenames (e.g. SFTP @baseFileName@ or settlement
-- CSV filename stem) that use that parser.
--
-- Nammayatri resolves the basename of the incoming report against this map and supplies the resulting
-- 'SplitSettlementCustomerType' to 'parsePaymentSettlementCsv' / 'parseAndEnrichPaymentSettlementCsv'
-- at the call site; mobility-core does not look this up automatically.
newtype SettlementParserTypeMap = SettlementParserTypeMap
  { unSettlementParserTypeMap :: Map.Map SplitSettlementCustomerType [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Per-provider service configuration (wraps SettlementSourceConfig)
-- ---------------------------------------------------------------------------

data SettlementServiceConfig = SettlementServiceConfig
  { settlementService :: SettlementService,
    sourceConfig :: SettlementSourceConfig,
    parserTypeMap :: Maybe SettlementParserTypeMap,
    useJuspayOrderStatus :: Maybe Bool,
    bankCode :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Source configuration (SFTP / Email)
-- ---------------------------------------------------------------------------

data SFTPConfig = SFTPConfig
  { host :: Text,
    port :: Int,
    username :: Text,
    password :: Maybe (EncryptedField 'AsEncrypted Text),
    remotePath :: Maybe Text,
    keyPath :: Maybe Text,
    privateKey64 :: Maybe Text,
    limit :: Maybe Int,
    csvChunkRowLimit :: Maybe Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data EmailConfig = EmailConfig
  { imapHost :: Text,
    imapPort :: Int,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    folderName :: Text,
    subjectFilter :: Maybe Text,
    limit :: Maybe Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Direct-API pull of the Juspay portal transaction export.
--
-- Unlike SFTP/Email which deliver PG-produced settlement files, this hits
-- @<portalBaseUrl>/api/q/download@ with an OAuth Bearer token and receives the
-- portal's transaction CSV directly. The token has a ~90 day expiry and must
-- be rotated manually. @juspayMerchantId@ is the portal-side merchant id used
-- in the query's @midFilter@ (e.g. @"nammayatri"@), which is not necessarily
-- the same string as the internal merchant id.
data JuspayApiConfig = JuspayApiConfig
  { portalBaseUrl :: BaseUrl,
    oauthToken :: EncryptedField 'AsEncrypted Text,
    juspayMerchantId :: Text,
    userAgent :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SettlementSourceConfig
  = SFTPSourceConfig SFTPConfig Text
  | EmailSourceConfig EmailConfig
  | JuspayApiSourceConfig JuspayApiConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data JuspayOrderStatusConfig = JuspayOrderStatusConfig
  { juspayBaseUrl :: BaseUrl,
    juspayApiKey :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
