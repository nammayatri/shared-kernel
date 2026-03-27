{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.YesBiz.PaymentParser
  ( parseYesBizCsv,
    parseYesBizRow,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import Data.Either (partitionEithers)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import Kernel.External.Settlement.Interface.Types
import Kernel.External.Settlement.YesBiz.PaymentTypes
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), HighPrecMoney)

parseYesBizCsv :: LBS.ByteString -> ParsePaymentSettlementResult
parseYesBizCsv csvData =
  case Csv.decodeByName csvData of
    Left err ->
      ParseResult
        { reports = [],
          totalRows = 0,
          failedRows = 0,
          errors = [T.pack $ "CSV parse error: " <> err]
        }
    Right (_, rows) ->
      let rowList = V.toList rows
          results = zipWith convertRow [1 :: Int ..] rowList
          (errs, goods) = partitionEithers results
       in ParseResult
            { reports = goods,
              totalRows = length rowList,
              failedRows = length errs,
              errors = errs
            }
  where
    convertRow idx row =
      case parseYesBizRow row of
        Left e -> Left $ "Row " <> show idx <> ": " <> e
        Right r -> Right r

parseYesBizRow :: YesBizRow -> Either Text PaymentSettlementReport
parseYesBizRow row = do
  txnType' <- parseTxnType row.transactionType
  txnStatus' <- parseTxnStatus row.status
  let txnDate' = parseDateTime row.transactionDate
      settlementDate' = parseDate row.settlementDate
      rawJson = A.toJSON row
  Right
    PaymentSettlementReport
      { orderId = row.orderId,
        txnId = nonEmpty' row.upiRequestId,
        rrn = nonEmpty' row.rrn,
        utr = nonEmpty' row.utrNumber,
        txnType = txnType',
        txnStatus = txnStatus',
        txnDate = txnDate',
        txnAmount = parseAmount row.transactionAmount,
        pgBaseFee = parseAmount row.fee,
        pgTax = parseAmount row.tax,
        settlementAmount = parseAmount row.credit,
        currency = INR,
        vendorId = nonEmpty' row.merchantId,
        uniqueSplitId = nonEmpty' row.transactionDescription,
        paymentGateway = Just "YESBIZ",
        paymentMethod = Just UPI,
        paymentMethodSubType = Nothing,
        settlementType = parseSettlementType row.txnType,
        settlementMode = Nothing,
        settlementId = Nothing,
        settlementDate = settlementDate',
        refundId = Nothing,
        refundArn = Nothing,
        refundDate = Nothing,
        refundAmount = if txnType' == REFUND then Just (parseAmount row.debit) else Nothing,
        refundBaseFee = Nothing,
        refundTax = Nothing,
        disputeId = Nothing,
        disputeType = Nothing,
        rawData = Just rawJson,
        cardIsin = Nothing,
        cardNetwork = Nothing,
        cardType = Nothing
      }

nonEmpty' :: Text -> Maybe Text
nonEmpty' t
  | T.null (T.strip t) = Nothing
  | otherwise = Just (T.strip t)

parseAmount :: Text -> HighPrecMoney
parseAmount t =
  case readMaybe (T.unpack $ T.strip t) of
    Just v -> v
    Nothing -> 0

-- | Parse datetime like "2026-01-01 0:4:55.631247"
--   Handles non-zero-padded hours/minutes (e.g., "0:4:55" instead of "00:04:55")
parseDateTime :: Text -> Maybe UTCTime
parseDateTime t
  | T.null (T.strip t) = Nothing
  | otherwise =
    let s = T.unpack $ T.strip t
        truncated = truncateFractional s
     in case parseTimeM True defaultTimeLocale "%Y-%m-%d %-H:%-M:%-S%Q" truncated of
          Just v -> Just v
          Nothing -> parseTimeM True defaultTimeLocale "%Y-%m-%d %-H:%-M:%-S" truncated

-- | Parse date like "2026-01-02"
parseDate :: Text -> Maybe UTCTime
parseDate t
  | T.null (T.strip t) = Nothing
  | otherwise = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack $ T.strip t)

-- | Truncate fractional seconds to avoid parse issues with long decimals
truncateFractional :: String -> String
truncateFractional s =
  case break (== '.') s of
    (before, '.' : after) ->
      let (frac, rest) = span (\c -> c >= '0' && c <= '9') after
       in before <> "." <> take 6 frac <> rest
    _ -> s

-- | SALE -> ORDER, REFUND -> REFUND
parseTxnType :: Text -> Either Text TxnType
parseTxnType t = case T.toUpper (T.strip t) of
  "SALE" -> Right ORDER
  "ORDER" -> Right ORDER
  "REFUND" -> Right REFUND
  other -> Left $ "Unknown transaction type: " <> other

-- | Status "00" means success for YesBiz UPI
parseTxnStatus :: Text -> Either Text TxnStatus
parseTxnStatus t = case T.strip t of
  "00" -> Right SUCCESS
  "0" -> Right SUCCESS
  "SUCCESS" -> Right SUCCESS
  "FAILED" -> Right FAILED
  other -> Left $ "Unknown transaction status: " <> other

parseSettlementType :: Text -> Maybe SettlementType
parseSettlementType t = case T.toUpper (T.strip t) of
  "PAY" -> Just CREDIT
  "CREDIT" -> Just CREDIT
  "DEBIT" -> Just DEBIT
  _ -> Nothing
