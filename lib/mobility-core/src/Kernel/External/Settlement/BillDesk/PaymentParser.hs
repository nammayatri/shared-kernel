{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.BillDesk.PaymentParser
  ( parseBillDeskCsv,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import Data.Either (partitionEithers)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import Kernel.External.Settlement.BillDesk.PaymentTypes
import Kernel.External.Settlement.Interface.Types
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), HighPrecMoney)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

parseBillDeskCsv :: LBS.ByteString -> ParsePaymentSettlementResult
parseBillDeskCsv csvData =
  let textContent = T.pack . map (toEnum . fromEnum) . LBS.unpack $ csvData
      allLines = T.lines textContent
      sections = splitSections allLines
      settledResults = parseSection parseSettledRow "SETTLED" sections.settled
      refundResults = parseSection parseRefundRow "REFUND" sections.refund
      chargebackResults = parseSection parseChargebackRow "CHARGEBACK" sections.chargeback
      (errs1, goods1) = partitionEithers settledResults
      (errs2, goods2) = partitionEithers refundResults
      (errs3, goods3) = partitionEithers chargebackResults
      allGoods = goods1 <> goods2 <> goods3
      allErrs = errs1 <> errs2 <> errs3
      total = length settledResults + length refundResults + length chargebackResults
   in ParseResult
        { reports = allGoods,
          totalRows = total,
          failedRows = length allErrs,
          errors = allErrs
        }

-- ---------------------------------------------------------------------------
-- Section splitting
-- ---------------------------------------------------------------------------

data SectionData = SectionData
  { settled :: [Text],
    refund :: [Text],
    chargeback :: [Text]
  }

splitSections :: [Text] -> SectionData
splitSections allLines =
  let settledLines = extractSection "SETTLED TRANSACTIONS" allLines
      refundLines = extractSection "REFUND TRANSACTIONS" allLines
      chargebackLines = extractSection "CHARGEBACK TRANSACTIONS" allLines
   in SectionData settledLines refundLines chargebackLines

extractSection :: Text -> [Text] -> [Text]
extractSection marker allLines =
  case dropWhile (not . isSectionMarker marker) allLines of
    [] -> []
    (_ : rest) ->
      case rest of
        (headerLine : dataLines) ->
          let rows = takeWhile isDataRow dataLines
           in if T.null (T.strip headerLine) then [] else headerLine : rows
        [] -> []

isSectionMarker :: Text -> Text -> Bool
isSectionMarker marker line = T.strip (head' (T.splitOn "," line)) == marker
  where
    head' [] = ""
    head' (x : _) = x

isDataRow :: Text -> Bool
isDataRow line =
  let stripped = T.strip line
   in not (T.null stripped)
        && not (isAllCommas stripped)
        && not (isSectionMarker "SETTLED TRANSACTIONS" line)
        && not (isSectionMarker "REFUND TRANSACTIONS" line)
        && not (isSectionMarker "CHARGEBACK TRANSACTIONS" line)
        && not (isSectionMarker "Notes" line)

isAllCommas :: Text -> Bool
isAllCommas = T.all (== ',')

-- ---------------------------------------------------------------------------
-- Per-section parsing via cassava
-- ---------------------------------------------------------------------------

parseSection ::
  (Csv.FromNamedRecord a) =>
  (a -> Int -> Either Text PaymentSettlementReport) ->
  Text ->
  [Text] ->
  [Either Text PaymentSettlementReport]
parseSection _ _ [] = []
parseSection convertFn sectionName sectionLines =
  let csvBytes = LBS.fromStrict . encodeUtf8 . T.unlines $ sectionLines
   in case Csv.decodeByName csvBytes of
        Left err ->
          [Left $ sectionName <> " CSV parse error: " <> T.pack err]
        Right (_, rows) ->
          let rowList = V.toList rows
           in zipWith (flip convertFn) [1 :: Int ..] rowList

-- ---------------------------------------------------------------------------
-- Row → PaymentSettlementReport converters
-- ---------------------------------------------------------------------------

parseSettledRow :: BillDeskSettledRow -> Int -> Either Text PaymentSettlementReport
parseSettledRow row idx = do
  let rawJson = A.toJSON row
  Right
    PaymentSettlementReport
      { orderId = row.ref1,
        txnId = nonEmpty' row.pgiRefNo,
        rrn = Nothing,
        utr = nonEmpty' row.bankRefNo,
        txnType = ORDER,
        txnStatus = SUCCESS,
        txnDate = parseDateTime row.dateOfTxn,
        txnAmount = parseAmount row.grossAmount,
        pgBaseFee = parseAmount row.charges,
        pgTax = parseAmount row.gst,
        settlementAmount = parseAmount row.netAmount,
        currency = INR,
        vendorId = nonEmpty' row.billerId,
        uniqueSplitId = Nothing,
        paymentGateway = Just "BILLDESK",
        paymentMethod = Nothing,
        paymentMethodSubType = Nothing,
        settlementType = Just CREDIT,
        settlementMode = Nothing,
        settlementId = nonEmpty' row.bankId,
        settlementDate = parseDateTime row.settlementDate,
        refundId = Nothing,
        refundArn = Nothing,
        refundDate = Nothing,
        refundAmount = Nothing,
        refundBaseFee = Nothing,
        refundTax = Nothing,
        disputeId = Nothing,
        disputeType = Nothing,
        rawData = Just rawJson,
        cardIsin = Nothing,
        cardNetwork = Nothing,
        cardType = Nothing
      }
  where
    _ = idx -- used in error context if needed later

parseRefundRow :: BillDeskRefundRow -> Int -> Either Text PaymentSettlementReport
parseRefundRow row idx = do
  let rawJson = A.toJSON row
  Right
    PaymentSettlementReport
      { orderId = row.ref1,
        txnId = nonEmpty' row.pgiRefNo,
        rrn = Nothing,
        utr = nonEmpty' row.bankRefNo,
        txnType = REFUND,
        txnStatus = SUCCESS,
        txnDate = parseDateTime row.dateOfTxn,
        txnAmount = parseAmount row.grossAmount,
        pgBaseFee = 0,
        pgTax = 0,
        settlementAmount = parseAmount row.grossAmount,
        currency = INR,
        vendorId = nonEmpty' row.billerId,
        uniqueSplitId = Nothing,
        paymentGateway = Just "BILLDESK",
        paymentMethod = Nothing,
        paymentMethodSubType = Nothing,
        settlementType = Just DEBIT,
        settlementMode = Nothing,
        settlementId = nonEmpty' row.bankId,
        settlementDate = parseDateTime row.settlementDate,
        refundId = nonEmpty' row.refundId,
        refundArn = Nothing,
        refundDate = parseDateTime row.refundDate,
        refundAmount = Just (parseAmount row.refundAmount),
        refundBaseFee = Nothing,
        refundTax = Nothing,
        disputeId = Nothing,
        disputeType = Nothing,
        rawData = Just rawJson,
        cardIsin = Nothing,
        cardNetwork = Nothing,
        cardType = Nothing
      }
  where
    _ = idx

parseChargebackRow :: BillDeskChargebackRow -> Int -> Either Text PaymentSettlementReport
parseChargebackRow row idx = do
  let rawJson = A.toJSON row
  Right
    PaymentSettlementReport
      { orderId = row.ref1,
        txnId = nonEmpty' row.pgiRefNo,
        rrn = Nothing,
        utr = nonEmpty' row.bankRefNo,
        txnType = CHARGEBACK,
        txnStatus = SUCCESS,
        txnDate = parseDateTime row.dateOfTxn,
        txnAmount = parseAmount row.grossAmount,
        pgBaseFee = 0,
        pgTax = 0,
        settlementAmount = parseAmount row.grossAmount,
        currency = INR,
        vendorId = nonEmpty' row.billerId,
        uniqueSplitId = Nothing,
        paymentGateway = Just "BILLDESK",
        paymentMethod = Nothing,
        paymentMethodSubType = Nothing,
        settlementType = Just DEBIT,
        settlementMode = Nothing,
        settlementId = nonEmpty' row.bankId,
        settlementDate = parseDateTime row.settlementDate,
        refundId = Nothing,
        refundArn = Nothing,
        refundDate = Nothing,
        refundAmount = Nothing,
        refundBaseFee = Nothing,
        refundTax = Nothing,
        disputeId = nonEmpty' row.chargebackReason,
        disputeType = Just OTHER_DISPUTE,
        rawData = Just rawJson,
        cardIsin = Nothing,
        cardNetwork = Nothing,
        cardType = Nothing
      }
  where
    _ = idx

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

nonEmpty' :: Text -> Maybe Text
nonEmpty' t
  | T.null (T.strip t) = Nothing
  | otherwise = Just (T.strip t)

parseAmount :: Text -> HighPrecMoney
parseAmount t =
  case readMaybe (T.unpack $ T.strip t) of
    Just v -> v
    Nothing -> 0

parseDateTime :: Text -> Maybe UTCTime
parseDateTime t
  | T.null (T.strip t) = Nothing
  | otherwise =
    parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" (T.unpack $ T.strip t)
