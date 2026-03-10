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
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Kernel.External.Settlement.BillDesk.PaymentParser as BillDeskPayment
import qualified Kernel.External.Settlement.HyperPG.PaymentParser as HyperPGPayment
import qualified Kernel.External.Settlement.HyperPG.PayoutParser as HyperPGPayout
import Kernel.External.Settlement.Interface.Types as Reexport
import Kernel.External.Settlement.Types as Reexport

-- | Parse a payment settlement CSV based on the service provider.
parsePaymentSettlementCsv ::
  SettlementService ->
  LBS.ByteString ->
  ParsePaymentSettlementResult
parsePaymentSettlementCsv service csvData = case service of
  HyperPG -> HyperPGPayment.parseHyperPGCsv csvData
  BillDesk -> BillDeskPayment.parseBillDeskCsv csvData

-- | Parse a payout settlement CSV based on the service provider.
parsePayoutSettlementCsv ::
  SettlementService ->
  LBS.ByteString ->
  ParsePayoutSettlementResult
parsePayoutSettlementCsv service csvData = case service of
  HyperPG -> HyperPGPayout.parseHyperPGPayoutCsv csvData
  BillDesk -> ParseResult [] 0 0 ["Payout parsing not supported for BillDesk"]
