{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.CCAvenue.PaymentParser
  ( parseCCAvenueCsv,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import Data.Either (partitionEithers)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import Kernel.External.Settlement.CCAvenue.PaymentTypes
import Kernel.External.Settlement.Interface.Types
import Kernel.External.Settlement.Utils.ParserUtils (nonEmpty', parseAmount)
import Kernel.Prelude
import Kernel.Types.Common (Currency (..))

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

parseCCAvenueCsv :: LBS.ByteString -> ParsePaymentSettlementResult
parseCCAvenueCsv csvData =
  let textContent = T.pack . map (toEnum . fromEnum) . LBS.unpack $ csvData
      allLines = T.lines textContent
      sections = splitSections allLines
      payoutRows = parseSectionRows @CCAvenuePayoutRow "Payout Summary" sections.payout
      txnRows = parseSectionRows @CCAvenueTxnRow "Transaction Summary" sections.transaction
      mergedRows = zipMerge txnRows payoutRows
      results = zipWith (flip parseMergedRow) [1 ..] mergedRows
      (errs, goods) = partitionEithers results
   in ParseResult
        { reports = goods,
          totalRows = length results,
          failedRows = length errs,
          errors = errs
        }

-- ---------------------------------------------------------------------------
-- Section splitting
-- ---------------------------------------------------------------------------

data SectionData = SectionData
  { payout :: [Text],
    transaction :: [Text]
  }

splitSections :: [Text] -> SectionData
splitSections allLines =
  let payoutLines = extractSection "Payout Summary" allLines
      txnLines = extractSection "Transaction Summary Details" allLines
   in SectionData payoutLines txnLines

extractSection :: Text -> [Text] -> [Text]
extractSection marker allLines =
  case dropWhile (not . isSectionHeader marker) allLines of
    [] -> []
    (_ : rest) ->
      case rest of
        (headerLine : dataLines) ->
          let rows = takeWhile isDataRow dataLines
           in if T.null (T.strip headerLine) then [] else headerLine : rows
        [] -> []

isSectionHeader :: Text -> Text -> Bool
isSectionHeader marker line = marker `T.isInfixOf` line

isDataRow :: Text -> Bool
isDataRow line =
  let stripped = T.strip line
   in not (T.null stripped)
        && not (isAllCommas stripped)
        && not (isSectionHeader "Payout Summary" line)
        && not (isSectionHeader "Transaction Summary Details" line)

isAllCommas :: Text -> Bool
isAllCommas = T.all (== ',')

-- ---------------------------------------------------------------------------
-- Section parsing via cassava
-- ---------------------------------------------------------------------------

parseSectionRows :: (Csv.FromNamedRecord a) => Text -> [Text] -> [Either Text a]
parseSectionRows _ [] = []
parseSectionRows sectionName sectionLines =
  let csvBytes = LBS.fromStrict . encodeUtf8 . T.unlines $ sectionLines
   in case Csv.decodeByName csvBytes of
        Left err -> [Left $ sectionName <> " CSV parse error: " <> T.pack err]
        Right (_, rows) -> map Right (V.toList rows)

-- ---------------------------------------------------------------------------
-- Zip payout + transaction rows by index
-- ---------------------------------------------------------------------------

zipMerge :: [Either Text CCAvenueTxnRow] -> [Either Text CCAvenuePayoutRow] -> [CCAvenueMergedRow]
zipMerge txnResults payoutResults =
  let txnRows = [r | Right r <- txnResults]
      payoutRows = [r | Right r <- payoutResults]
      payoutPadded = map Just payoutRows <> repeat Nothing
   in zipWith (\t p -> CCAvenueMergedRow t p) txnRows payoutPadded

-- ---------------------------------------------------------------------------
-- Merged row → PaymentSettlementReport
-- ---------------------------------------------------------------------------

parseMergedRow :: CCAvenueMergedRow -> Int -> Either Text PaymentSettlementReport
parseMergedRow merged idx = do
  let row = merged.txnRow
      payout = merged.payoutRow
      rawJson = A.toJSON merged
      txnType' = parseCCAvenueTxnType row.txnType
      settlementType' = case txnType' of
        ORDER -> Just CREDIT
        REFUND -> Just DEBIT
        CHARGEBACK -> Just DEBIT
  Right
    PaymentSettlementReport
      { orderId = row.ccaOrderId, -- Merchant Subscription Transaction ID → Order ID
        txnId = nonEmpty' row.ccavenueRefNumber, -- Transaction ID → CCAvenue Ref Number
        rrn = nonEmpty' row.authCode, -- Bank RRN → Auth Code
        utr = payout >>= nonEmpty' . (.utrNo), -- Settlement UTR → Payout Summary - UTR No
        txnType = txnType', -- Document Type → Transaction Type
        txnStatus = SUCCESS,
        txnDate = parseCCAvenueDateTime row.txnDate, -- Transaction Date → Date
        txnAmount = parseAmount row.amount, -- Charged Amount → Amount
        pgBaseFee = parseAmount row.tdrAmount, -- PG Fee → TDR Amount
        pgTax = parseAmount row.tax, -- GST on PG Fee → Tax
        settlementAmount = parseAmount row.payableAmount, -- Net Settled Amount → Payable Amount
        currency = INR,
        vendorId = nonEmpty' row.merchantId, -- MID → Merchant ID
        uniqueSplitId = Nothing,
        paymentGateway = Just "HDFC-CCAVENUE", -- PG name → Hardcode as HDFC-CCAvenue
        paymentMethod = parseCCAvenuePaymentMethod row.paymentType, -- Paymode → Payment Type
        paymentMethodSubType = Nothing,
        settlementType = settlementType',
        settlementMode = Nothing,
        settlementId = payout >>= nonEmpty' . (.payId), -- Settlement ID → Payout Summary - Pay ID
        settlementDate = parseCCAvenueDateTime row.settlementDate, -- Settlement Date → Settlement Date
        pgApprovalCode = nonEmpty' row.ccavenueRefNumber, -- PG Approval code → CCAvenue Ref Number
        pgRequestId = nonEmpty' row.invoiceRefNo, -- PG Request id → Invoice Reference No
        bankId = Nothing, -- Bank ID → #N/A
        refundId = Nothing,
        refundArn = Nothing, -- ARN → PG does not provide
        refundDate = Nothing,
        refundAmount = Nothing,
        refundBaseFee = Nothing,
        refundTax = Nothing,
        disputeId = Nothing,
        disputeType = Nothing,
        rawData = Just rawJson,
        cardIsin = Nothing,
        cardNetwork = Nothing,
        cardType = nonEmpty' row.cardName, -- Card Type → Card Name
        isOffer = Nothing,
        offerCode = Nothing,
        offerId = Nothing,
        actualAmount = Nothing,
        cardNumber = Nothing
      }
  where
    _ = idx

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

parseCCAvenuePaymentMethod :: Text -> Maybe PaymentMethodType
parseCCAvenuePaymentMethod t = case T.toLower (T.strip t) of
  "credit card" -> Just CREDIT_CARD
  "debit card" -> Just DEBIT_CARD
  "net banking" -> Just NETBANKING
  "upi" -> Just UPI
  "wallet" -> Just WALLET
  "bharat qr" -> Just BHARAT_QR
  "credit card emi" -> Just EMI
  "debit card emi" -> Just EMI
  "commercial card" -> Just COMMERCIAL_CARD
  "pay later" -> Just PAY_LATER
  _ -> Nothing

parseCCAvenueTxnType :: Text -> TxnType
parseCCAvenueTxnType t = case T.toLower (T.strip t) of
  "payment" -> ORDER
  "sale" -> ORDER
  "refund" -> REFUND
  "chargeback" -> CHARGEBACK
  _ -> ORDER

parseCCAvenueDateTime :: Text -> Maybe UTCTime
parseCCAvenueDateTime t
  | T.null (T.strip t) = Nothing
  | otherwise =
    parseTimeM True defaultTimeLocale "%d/%m/%Y %H:%M:%S" (T.unpack $ T.strip t)
      <|> parseTimeM True defaultTimeLocale "%d-%m-%Y %H:%M:%S" (T.unpack $ T.strip t)
      <|> parseTimeM True defaultTimeLocale "%d/%m/%Y" (T.unpack $ T.strip t)
      <|> parseTimeM True defaultTimeLocale "%d/%m/%y" (T.unpack $ T.strip t)
