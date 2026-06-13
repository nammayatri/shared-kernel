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
import qualified Data.Vector as V
import Kernel.External.Settlement.BillDesk.PaymentTypes
import Kernel.External.Settlement.Interface.Types
import Kernel.External.Settlement.Utils.ParserUtils (nonEmpty', parseAmount, parseDateTime)
import Kernel.Prelude
import Kernel.Types.Common (Currency (..))

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
      { orderId = row.ref1, -- Merchant Subscription Transaction ID → Ref. 1
        txnId = nonEmpty' row.pgiRefNo, -- Transaction ID → PGI Ref. No.
        rrn = nonEmpty' row.bankRefNo, -- Bank RRN → Bank Ref. No.
        utr = Nothing, -- Settlement UTR → #N/A
        txnType = ORDER, -- Document Type → SETTLED TRANSACTIONS= Payment
        txnStatus = SUCCESS,
        txnDate = parseDateTime row.dateOfTxn, -- Transaction Date → Date of Txn
        txnAmount = parseAmount row.grossAmount, -- Charged Amount → Gross Amount(Rs.Ps)
        pgBaseFee = parseAmount row.charges, -- PG Fee → Charges (Rs.Ps)
        pgTax = parseAmount row.gst, -- GST on PG Fee → GST (Rs Ps)
        settlementAmount = parseAmount row.netAmount, -- Net Settled Amount → Net Amount(Rs.Ps)
        currency = INR,
        vendorId = nonEmpty' row.billerId, -- MID → Biller Id
        uniqueSplitId = Nothing,
        paymentGateway = Just "BILLDESK", -- PG name → Hardcode as Billdesk
        paymentMethod = parseBillDeskPaymentMethod row.payRef2, -- Paymode → Pay Ref1                             ---------mapping correction needed
        paymentMethodSubType = Nothing,
        settlementType = Just CREDIT,
        settlementMode = Nothing,
        settlementId = Nothing, -- Settlement ID → #N/A
        settlementDate = parseDateTime row.settlementDate, -- Settlement Date → Settlement Date
        pgApprovalCode = nonEmpty' row.pgiRefNo, -- PG Approval code → PGI Ref. No.
        pgRequestId = nonEmpty' row.ref5, -- PG Request id → Ref. 5
        bankId = nonEmpty' row.bankId, -- Bank ID → Bank Id
        refundId = Nothing,
        refundArn = Nothing, -- ARN → #N/A
        refundDate = Nothing,
        refundAmount = Nothing,
        refundBaseFee = Nothing,
        refundTax = Nothing,
        disputeId = Nothing,
        disputeType = Nothing,
        rawData = Just rawJson,
        cardIsin = Nothing,
        cardNetwork = Nothing,
        cardType = Nothing, -- Card Type → #N/A
        cardNumber = Nothing,
        isOffer = Nothing,
        offerCode = Nothing,
        offerId = Nothing,
        actualAmount = Nothing
      }
  where
    _ = idx

parseRefundRow :: BillDeskRefundRow -> Int -> Either Text PaymentSettlementReport
parseRefundRow row idx = do
  let rawJson = A.toJSON row
  Right
    PaymentSettlementReport
      { orderId = row.ref1, -- Merchant Subscription Transaction ID → Ref. 1
        txnId = nonEmpty' row.pgiRefNo, -- Transaction ID → PGI Ref. No.
        rrn = nonEmpty' row.bankRefNo, -- Bank RRN → Bank Ref. No.
        utr = Nothing, -- Settlement UTR → #N/A
        txnType = REFUND, -- Document Type → REFUND TRANSACTIONS= Refund
        txnStatus = SUCCESS,
        txnDate = parseDateTime row.dateOfTxn, -- Transaction Date → Refund Date
        txnAmount = parseAmount row.grossAmount, -- Charged Amount → Refund Amount (Rs. Ps.)
        pgBaseFee = 0, -- PG Fee → #N/A
        pgTax = 0, -- GST on PG Fee → #N/A
        settlementAmount = parseAmount row.grossAmount, -- Net Settled Amount → Refund Amount (Rs. Ps.)
        currency = INR,
        vendorId = nonEmpty' row.billerId, -- MID → Biller Id
        uniqueSplitId = Nothing,
        paymentGateway = Just "BILLDESK", -- PG name → Hardcode as Billdesk
        paymentMethod = parseBillDeskPaymentMethod row.ref2, -- Paymode → Ref2
        paymentMethodSubType = Nothing,
        settlementType = Just DEBIT,
        settlementMode = Nothing,
        settlementId = Nothing, -- Settlement ID → #N/A
        settlementDate = parseDateTime row.settlementDate, -- Settlement Date → Settlement Date of Settled Transactions
        pgApprovalCode = nonEmpty' row.pgiRefNo, -- PG Approval code → PGI Ref. No.
        pgRequestId = nonEmpty' row.ref5, -- PG Request id → Ref. 5
        bankId = nonEmpty' row.bankId, -- Bank ID → Bank Id
        refundId = Nothing,
        refundArn = nonEmpty' row.refundId, -- ARN → Refund ID
        refundDate = parseDateTime row.refundDate,
        refundAmount = Just (parseAmount row.refundAmount),
        refundBaseFee = Nothing,
        refundTax = Nothing,
        disputeId = Nothing,
        disputeType = Nothing,
        rawData = Just rawJson,
        cardIsin = Nothing,
        cardNetwork = Nothing,
        cardType = Nothing, -- Card Type → #N/A
        isOffer = Nothing,
        offerCode = Nothing,
        offerId = Nothing,
        actualAmount = Nothing,
        cardNumber = Nothing
      }
  where
    _ = idx

parseChargebackRow :: BillDeskChargebackRow -> Int -> Either Text PaymentSettlementReport
parseChargebackRow row idx = do
  let rawJson = A.toJSON row
  Right
    PaymentSettlementReport
      { orderId = row.ref1, -- Merchant Subscription Transaction ID → Ref. 1
        txnId = nonEmpty' row.pgiRefNo, -- Transaction ID → PGI Ref. No.
        rrn = nonEmpty' row.bankRefNo, -- Bank RRN → Bank Ref. No.
        utr = Nothing, -- Settlement UTR → #N/A
        txnType = CHARGEBACK, -- Document Type → CHARGEBACK TRANSACTIONS= Chargeback
        txnStatus = SUCCESS,
        txnDate = parseDateTime row.dateOfTxn, -- Transaction Date → Chargeback date
        txnAmount = parseAmount row.grossAmount, -- Charged Amount → Chargeback Amount (Rs Ps)
        pgBaseFee = 0, -- PG Fee → #N/A
        pgTax = 0, -- GST on PG Fee → #N/A
        settlementAmount = parseAmount row.grossAmount, -- Net Settled Amount → #N/A (using gross)
        currency = INR,
        vendorId = nonEmpty' row.billerId, -- MID → Biller Id
        uniqueSplitId = Nothing,
        paymentGateway = Just "BILLDESK", -- PG name → Hardcode as Billdesk
        paymentMethod = parseBillDeskPaymentMethod row.ref2, -- Paymode → Ref2
        paymentMethodSubType = Nothing,
        settlementType = Just DEBIT,
        settlementMode = Nothing,
        settlementId = Nothing, -- Settlement ID → #N/A
        settlementDate = parseDateTime row.settlementDate, -- Settlement Date → Settlement Date of Settled Transactions
        pgApprovalCode = nonEmpty' row.pgiRefNo, -- PG Approval code → PGI Ref. No.
        pgRequestId = nonEmpty' row.ref5, -- PG Request id → Ref. 5
        bankId = nonEmpty' row.bankId, -- Bank ID → Bank Id
        refundId = Nothing,
        refundArn = Nothing, -- ARN → #N/A
        refundDate = Nothing,
        refundAmount = Nothing,
        refundBaseFee = Nothing,
        refundTax = Nothing,
        disputeId = nonEmpty' row.chargebackReason,
        disputeType = Just OTHER_DISPUTE,
        rawData = Just rawJson,
        cardIsin = Nothing,
        cardNetwork = Nothing,
        cardType = Nothing, -- Card Type → #N/A
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

parseBillDeskPaymentMethod :: Text -> Maybe PaymentMethodType
parseBillDeskPaymentMethod t = case T.strip t of
  "1" -> Just NETBANKING
  "2" -> Just CREDIT_CARD
  "3" -> Just DEBIT_CARD
  "4" -> Just CASH_CARD
  "5" -> Just WALLET
  "10" -> Just UPI
  "11" -> Just BHARAT_QR
  "12" -> Just EMI
  "13" -> Just NEFT
  "18" -> Just UPI_CREDIT
  "19" -> Just ENACH
  "20" -> Just CBDC
  "21" -> Just UPI_PREPAID_WALLET
  "22" -> Just UPI_CREDIT_LINE
  _ -> Nothing
