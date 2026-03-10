{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.HyperPG.PayoutParser
  ( parseHyperPGPayoutCsv,
    parseHyperPGPayoutRow,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import Data.Either (partitionEithers)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import Kernel.External.Settlement.HyperPG.PayoutTypes
import Kernel.External.Settlement.Interface.Types
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), HighPrecMoney)

parseHyperPGPayoutCsv :: LBS.ByteString -> ParsePayoutSettlementResult
parseHyperPGPayoutCsv csvData =
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
      case parseHyperPGPayoutRow row of
        Left e -> Left $ "Row " <> show idx <> ": " <> e
        Right r -> Right r

parseHyperPGPayoutRow :: HyperPGPayoutRow -> Either Text PayoutSettlementReport
parseHyperPGPayoutRow row = do
  txnStatus' <- parseTxnStatus row.fulfillmentTxnStatus
  let txnDate' = parseISODateTime row.fulfillmentTxnCreatedAt
      fulfillmentDate' = parseISODateTime row.fulfillmentCreatedAt
      rawJson = A.toJSON row
      (mbIfsc, mbAcct) = parseBeneficiaryDetails row.beneficiaryDetails
  Right
    PayoutSettlementReport
      { orderId = row.orderId,
        txnId = nonEmpty' row.fulfillmentTxnUuid,
        rrn = nonEmpty' row.gatewayRefId,
        utr = nonEmpty' row.transactionReference,
        txnStatus = txnStatus',
        txnDate = txnDate',
        txnAmount = parseAmount row.amount,
        settlementAmount = parseAmount row.amount,
        currency = INR,
        payoutCustomerId = row.customerId,
        paymentGateway = nonEmpty' row.gateway,
        beneficiaryIfsc = mbIfsc,
        beneficiaryAccountNumber = mbAcct,
        beneficiaryType = Just "ACCOUNT_IFSC",
        bankName = nonEmpty' row.bankName,
        settlementType = Just CREDIT,
        settlementMode = Nothing,
        settlementId = Nothing,
        settlementDate = Nothing,
        payoutRequestId = Nothing,
        fulfillmentId = nonEmpty' row.fulfillmentUuid,
        fulfillmentInstrumentType = parseFulfillmentInstrument row.fulfillmentInstrumentType,
        fulfillmentMethod = nonEmpty' row.fulfillmentMethod,
        fulfillmentStatus = nonEmpty' row.fulfillmentStatus,
        fulfillmentResponseCode = nonEmpty' row.responseCode,
        fulfillmentResponseMessage = nonEmpty' row.responseMessage,
        fulfillmentDate = fulfillmentDate',
        fulfillmentAmount = Just (parseAmount row.amount),
        rawData = Just rawJson
      }

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

parseTxnStatus :: Text -> Either Text TxnStatus
parseTxnStatus t = case T.toUpper (T.strip t) of
  "SUCCESS" -> Right SUCCESS
  "FAILED" -> Right FAILED
  other -> Left $ "Unknown transaction status: " <> other

parseISODateTime :: Text -> Maybe UTCTime
parseISODateTime t
  | T.null (T.strip t) = Nothing
  | otherwise =
    let s = T.unpack (T.strip t)
     in parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" s
          <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" s

parseFulfillmentInstrument :: Text -> Maybe FulfillmentInstrument
parseFulfillmentInstrument t = case T.toUpper (T.strip t) of
  "ACCOUNT_IFSC" -> Just NEFT
  "IMPS" -> Just IMPS
  "NEFT" -> Just NEFT
  "RTGS" -> Just RTGS
  "UPI" -> Just FI_UPI
  "" -> Nothing
  _ -> Just OTHER_INSTRUMENT

-- | Extract IFSC and account number from the JSON beneficiary_details field.
-- Format: {"type":"ACCOUNT_IFSC","details":{"ifsc":"SBINXXXXXXX","account":"XXXXXXXX528"}}
parseBeneficiaryDetails :: Text -> (Maybe Text, Maybe Text)
parseBeneficiaryDetails t
  | T.null (T.strip t) = (Nothing, Nothing)
  | otherwise =
    case A.decodeStrict (encodeUtf8 t) :: Maybe A.Value of
      Just (A.Object obj) ->
        case KM.lookup "details" obj of
          Just (A.Object details) ->
            let mbIfsc = case KM.lookup "ifsc" details of
                  Just (A.String v) -> Just v
                  _ -> Nothing
                mbAcct = case KM.lookup "account" details of
                  Just (A.String v) -> Just v
                  _ -> Nothing
             in (mbIfsc, mbAcct)
          _ -> (Nothing, Nothing)
      _ -> (Nothing, Nothing)
