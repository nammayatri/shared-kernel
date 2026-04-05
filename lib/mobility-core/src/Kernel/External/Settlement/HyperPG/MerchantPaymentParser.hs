{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.HyperPG.MerchantPaymentParser
  ( parseHyperPGMerchantCsv,
    parseHyperPGMerchantRow,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import Data.Either (partitionEithers)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import Kernel.External.Settlement.HyperPG.MerchantPaymentTypes
import Kernel.External.Settlement.Interface.Types
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), HighPrecMoney)

parseHyperPGMerchantCsv :: LBS.ByteString -> ParsePaymentSettlementResult
parseHyperPGMerchantCsv csvData =
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
          (errs, maybeGoods) = partitionEithers results
          goods = catMaybes maybeGoods
       in ParseResult
            { reports = goods,
              totalRows = length rowList,
              failedRows = length errs,
              errors = errs
            }
  where
    convertRow idx row =
      case parseHyperPGMerchantRow row of
        Left e -> Left $ "Row " <> show idx <> ": " <> e
        Right r -> Right r

parseHyperPGMerchantRow :: HyperPGMerchantRow -> Either Text (Maybe PaymentSettlementReport)
parseHyperPGMerchantRow row = do
  txnType' <- parseTxnType row.transactionType
  txnStatus' <- parseTxnStatus row.transactionStatus
  let settlementType' = parseSettlementType row.txnSettlementType
      settlementMode' = parseSettlementMode row.settlementMode
      paymentMethod' = parsePaymentMethod row.paymentMethodType
      txnDate' = parseDateTime row.transactionDate
      refundDate' = parseDateTime row.refundDate
      settlementDate' = parseDateTime row.settlementDate
      rawJson = A.toJSON row
      totalSettlement = parseAmount row.settlementAmount
      vendorTotal = computeVendorTotal row.transactionSplits
      merchantAmt = totalSettlement - vendorTotal
  if merchantAmt == 0
    then Right Nothing
    else
      Right $
        Just
          PaymentSettlementReport
            { orderId = row.orderId,
              txnId = nonEmpty' row.transactionId,
              rrn = nonEmpty' row.rrn,
              utr = nonEmpty' row.utr,
              txnType = txnType',
              txnStatus = txnStatus',
              txnDate = txnDate',
              txnAmount = parseAmount row.amount,
              pgBaseFee = parseAmount row.fee,
              pgTax = parseAmount row.tax,
              settlementAmount = merchantAmt,
              currency = INR,
              vendorId = Nothing,
              uniqueSplitId = Nothing,
              paymentGateway = Just "HYPERPG",
              paymentMethod = paymentMethod',
              paymentMethodSubType = nonEmpty' row.paymentMethodSubType,
              settlementType = settlementType',
              settlementMode = settlementMode',
              settlementId = nonEmpty' row.settlementId,
              settlementDate = settlementDate',
              refundId = nonEmpty' row.refundId,
              refundArn = nonEmpty' row.refundArn,
              refundDate = refundDate',
              refundAmount = if txnType' == REFUND then Just (parseAmount row.amount) else Nothing,
              refundBaseFee = if txnType' == REFUND then Just (parseAmount row.fee) else Nothing,
              refundTax = if txnType' == REFUND then Just (parseAmount row.tax) else Nothing,
              disputeId = nonEmpty' row.disputeId,
              disputeType = parseDisputeType row.disputeType,
              rawData = Just rawJson,
              cardIsin = Nothing,
              cardNetwork = Nothing,
              cardType = Nothing,
              isOffer = Nothing,
              offerCode = Nothing,
              offerId = Nothing,
              actualAmount = Nothing
            }

computeVendorTotal :: Text -> HighPrecMoney
computeVendorTotal splitsText
  | T.null (T.strip splitsText) = 0
  | otherwise =
    case A.eitherDecodeStrict (TE.encodeUtf8 $ T.strip splitsText) of
      Left _ -> 0
      Right splits -> sum $ map (parseAmount . grossAmount) (computedVendorsSplits splits)

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
  | otherwise = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack $ T.strip t)

parseTxnType :: Text -> Either Text TxnType
parseTxnType t = case T.toLower (T.strip t) of
  "order" -> Right ORDER
  "refund" -> Right REFUND
  other -> Left $ "Unknown transaction type: " <> other

parseTxnStatus :: Text -> Either Text TxnStatus
parseTxnStatus t = case T.toUpper (T.strip t) of
  "SUCCESS" -> Right SUCCESS
  "FAILED" -> Right FAILED
  other -> Left $ "Unknown transaction status: " <> other

parseSettlementType :: Text -> Maybe SettlementType
parseSettlementType t = case T.toUpper (T.strip t) of
  "CREDIT" -> Just CREDIT
  "DEBIT" -> Just DEBIT
  _ -> Nothing

parseSettlementMode :: Text -> Maybe SettlementMode
parseSettlementMode t = case T.toUpper (T.strip t) of
  "GROSS" -> Just GROSS
  "NET" -> Just NET
  "NETTING" -> Just NETTING
  _ -> Nothing

parsePaymentMethod :: Text -> Maybe PaymentMethodType
parsePaymentMethod t = case T.toUpper (T.strip t) of
  "UPI" -> Just UPI
  "CREDIT_CARD" -> Just CREDIT_CARD
  "DEBIT_CARD" -> Just DEBIT_CARD
  "NETBANKING" -> Just NETBANKING
  "WALLET" -> Just WALLET
  _ -> Nothing

parseDisputeType :: Text -> Maybe DisputeType
parseDisputeType t = case T.toUpper (T.strip t) of
  "FRAUD" -> Just FRAUD
  "CONSUMER" -> Just CONSUMER
  "PROCESSING_ERROR" -> Just PROCESSING_ERROR
  "OTHER_DISPUTE" -> Just OTHER_DISPUTE
  _ -> Nothing
