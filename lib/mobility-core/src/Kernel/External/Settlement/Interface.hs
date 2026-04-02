{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.Interface
  ( module Reexport,
    parsePaymentSettlementCsv,
    parsePayoutSettlementCsv,
    fetchSettlementCsv,
    fetchAndParsePaymentSettlement,
    fetchAndParsePayoutSettlement,
  )
where

import qualified Data.ByteString.Lazy as LBS
import Kernel.External.Encryption
import qualified Kernel.External.Settlement.BillDesk.PaymentParser as BillDeskPayment
import qualified Kernel.External.Settlement.HyperPG.PaymentParser as HyperPGPayment
import qualified Kernel.External.Settlement.HyperPG.PayoutParser as HyperPGPayout
import Kernel.External.Settlement.Interface.Types as Reexport
import qualified Kernel.External.Settlement.Sources.Email as EmailSource
import qualified Kernel.External.Settlement.Sources.SFTP as SFTPSource
import Kernel.External.Settlement.Types as Reexport
import Kernel.Prelude

-- ---------------------------------------------------------------------------
-- Pure parsers (CSV ByteString → ParseResult)
-- ---------------------------------------------------------------------------

parsePaymentSettlementCsv ::
  SettlementService ->
  LBS.ByteString ->
  ParsePaymentSettlementResult
parsePaymentSettlementCsv service csvData = case service of
  HyperPG -> HyperPGPayment.parseHyperPGCsv csvData
  BillDesk -> BillDeskPayment.parseBillDeskCsv csvData

parsePayoutSettlementCsv ::
  SettlementService ->
  LBS.ByteString ->
  ParsePayoutSettlementResult
parsePayoutSettlementCsv service csvData = case service of
  HyperPG -> HyperPGPayout.parseHyperPGPayoutCsv csvData
  BillDesk -> ParseResult [] 0 0 ["Payout parsing not supported for BillDesk"]

-- ---------------------------------------------------------------------------
-- Source fetch (Email / SFTP → CSV ByteString)
-- ---------------------------------------------------------------------------

-- | Fetch CSV data from the configured source (Email or SFTP).
fetchSettlementCsv ::
  (EncFlow m r, MonadIO m) =>
  SettlementSourceConfig ->
  m (Either Text LBS.ByteString)
fetchSettlementCsv (EmailSourceConfig emailCfg) =
  EmailSource.fetchSettlementFile emailCfg
fetchSettlementCsv (SFTPSourceConfig sftpCfg filePattern) =
  SFTPSource.fetchSettlementFile sftpCfg filePattern

-- ---------------------------------------------------------------------------
-- End-to-end: fetch from source + parse
-- ---------------------------------------------------------------------------

-- | Fetch CSV from source and parse as a payment settlement report.
fetchAndParsePaymentSettlement ::
  (EncFlow m r, MonadIO m) =>
  SettlementSourceConfig ->
  SettlementService ->
  m (Either Text ParsePaymentSettlementResult)
fetchAndParsePaymentSettlement sourceConfig service = do
  csvResult <- fetchSettlementCsv sourceConfig
  pure $ parsePaymentSettlementCsv service <$> csvResult

-- | Fetch CSV from source and parse as a payout settlement report.
fetchAndParsePayoutSettlement ::
  (EncFlow m r, MonadIO m) =>
  SettlementSourceConfig ->
  SettlementService ->
  m (Either Text ParsePayoutSettlementResult)
fetchAndParsePayoutSettlement sourceConfig service = do
  csvResult <- fetchSettlementCsv sourceConfig
  pure $ parsePayoutSettlementCsv service <$> csvResult
