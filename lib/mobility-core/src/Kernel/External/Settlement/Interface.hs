{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.Interface
  ( parsePaymentSettlementCsv,
    parsePayoutSettlementCsv,
    parseAndEnrichPaymentSettlementCsv,
    module Reexport,
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified EulerHS.Language as L
import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.Settlement.BillDesk.PaymentParser as BillDeskPayment
import qualified Kernel.External.Settlement.HyperPG.MerchantPaymentParser as HyperPGMerchantPayment
import qualified Kernel.External.Settlement.HyperPG.PaymentParser as HyperPGPayment
import qualified Kernel.External.Settlement.HyperPG.PayoutParser as HyperPGPayout
import Kernel.External.Settlement.Interface.Types as Reexport
import Kernel.External.Settlement.Types as Reexport
import Kernel.External.Settlement.Utils.JuspayOrderStatus as Reexport
import qualified Kernel.External.Settlement.YesBiz.PaymentParser as YesBizPayment
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Servant.Client (HasRequestId)

-- | Parse a payment settlement CSV. For 'HyperPG', use 'Just' 'MERCHANT' for MPR-style merchant reports.
parsePaymentSettlementCsv ::
  SettlementService ->
  Maybe SplitSettlementCustomerType ->
  LBS.ByteString ->
  ParsePaymentSettlementResult
parsePaymentSettlementCsv settlementService mbSplit csvData = case settlementService of
  HyperPG -> case fromMaybe VENDOR mbSplit of
    VENDOR -> HyperPGPayment.parseHyperPGCsv csvData
    MERCHANT -> HyperPGMerchantPayment.parseHyperPGMerchantCsv csvData
  BillDesk -> BillDeskPayment.parseBillDeskCsv csvData
  YesBiz -> YesBizPayment.parseYesBizCsv csvData

parsePayoutSettlementCsv ::
  SettlementService ->
  LBS.ByteString ->
  ParsePayoutSettlementResult
parsePayoutSettlementCsv settlementService csvData = case settlementService of
  HyperPG -> HyperPGPayout.parseHyperPGPayoutCsv csvData
  BillDesk -> ParseResult [] 0 0 ["Payout parsing not supported for BillDesk"]
  YesBiz -> ParseResult [] 0 0 ["Payout parsing not supported for YesBiz"]

-- | Parse payment CSV bytes, then Juspay-enrich each row when enabled in 'SettlementServiceConfig'
-- ('juspayOrderStatusEnabled', URL, API key). Fetch of bytes is the caller's responsibility.
parseAndEnrichPaymentSettlementCsv ::
  ( EncFlow m r,
    Metrics.CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m,
    MonadIO m
  ) =>
  SettlementServiceConfig ->
  LBS.ByteString ->
  m ParsePaymentSettlementResult
parseAndEnrichPaymentSettlementCsv config csvBytes = do
  let parsed0 =
        parsePaymentSettlementCsv
          config.settlementService
          config.splitSettlementCustomerType
          csvBytes
  enrichedReports <- mapM (enrichPaymentReport config) (reports parsed0)
  pure parsed0 {reports = catMaybes enrichedReports}
