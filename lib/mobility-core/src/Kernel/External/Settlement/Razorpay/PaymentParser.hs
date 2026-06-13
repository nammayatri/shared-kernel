{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.Razorpay.PaymentParser
  ( parseRazorpayCsv,
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
import Kernel.External.Settlement.Interface.Types
import Kernel.External.Settlement.Razorpay.PaymentTypes
import Kernel.External.Settlement.Utils.ParserUtils (nonEmpty', parseAmount)
import Kernel.Prelude
import Kernel.Types.Common (Currency (..))

parseRazorpayCsv :: LBS.ByteString -> ParsePaymentSettlementResult
parseRazorpayCsv csvData =
  case Csv.decodeByName csvData of
    Left err ->
      ParseResult [] 0 0 ["Razorpay CSV parse error: " <> T.pack err]
    Right (_, rows) ->
      let rowList = V.toList rows
          results = zipWith (flip parseRazorpayRow) [1 ..] rowList
          (errs, goods) = partitionEithers results
       in ParseResult
            { reports = goods,
              totalRows = length results,
              failedRows = length errs,
              errors = errs
            }

parseRazorpayRow :: RazorpayRow -> Int -> Either Text PaymentSettlementReport
parseRazorpayRow row idx = do
  let rawJson = A.toJSON row
      txnType' = parseRazorpayTxnType row.transactionEntity
      settlementType' = case txnType' of
        ORDER -> Just CREDIT
        REFUND -> Just DEBIT
        CHARGEBACK -> Just DEBIT
  Right
    PaymentSettlementReport
      { orderId = row.orderReceipt, -- Merchant Subscription Transaction ID → order_receipt
        txnId = nonEmpty' row.entityId, -- Transaction ID → entity_id
        rrn = nonEmpty' row.rrn, -- Bank RRN → RRN
        utr = nonEmpty' row.settlementUtr, -- Settlement UTR → settlement_utr
        txnType = txnType', -- Document Type → transaction_entity
        txnStatus = SUCCESS,
        txnDate = parseRazorpayDateTime row.paymentCapturedAt, -- Transaction Date → payment_captured_at
        txnAmount = parseAmount row.amount, -- Charged Amount → amount
        pgBaseFee = parseAmount row.fee, -- PG Fee → fee (exclusive tax)
        pgTax = parseAmount row.tax, -- GST on PG Fee → tax
        settlementAmount = parseAmount row.credit, -- Net Settled Amount → credit
        currency = INR,
        vendorId = Nothing, -- MID → Hardcode MID (set at domain layer)                ---- see into it
        uniqueSplitId = Nothing,
        paymentGateway = Just "RAZORPAY", -- PG name → Hardcode as Razorpay
        paymentMethod = parseRazorpayPaymentMethod row.paymentMethod, -- Paymode → payment_method
        paymentMethodSubType = Nothing,
        settlementType = settlementType',
        settlementMode = Nothing,
        settlementId = nonEmpty' row.settlementId, -- Settlement ID → settlement_id
        settlementDate = parseRazorpayDateTime row.settledAt, -- Settlement Date → settled_at
        pgApprovalCode = nonEmpty' row.entityId, -- PG Approval code → entity_id
        pgRequestId = nonEmpty' row.orderId, -- PG Request id → order_id
        bankId = nonEmpty' row.issuerName, -- Bank ID → issuer_name
        refundId = Nothing,
        refundArn = nonEmpty' row.arn, -- ARN → arn
        refundDate = Nothing,
        refundAmount = Nothing,
        refundBaseFee = Nothing,
        refundTax = Nothing,
        disputeId = Nothing,
        disputeType = Nothing,
        rawData = Just rawJson,
        cardIsin = Nothing,
        cardNetwork = Nothing,
        cardType = nonEmpty' row.cardType, -- Card Type → card_type
        isOffer = Nothing,
        offerCode = Nothing,
        offerId = Nothing,
        actualAmount = Nothing,
        cardNumber = nonEmpty' row.vpa
      }
  where
    _ = idx

parseRazorpayTxnType :: Text -> TxnType
parseRazorpayTxnType t = case T.toLower (T.strip t) of
  "payment" -> ORDER
  "refund" -> REFUND
  _ -> ORDER

parseRazorpayPaymentMethod :: Text -> Maybe PaymentMethodType
parseRazorpayPaymentMethod t = case T.toLower (T.strip t) of
  "upi" -> Just UPI
  "credit card" -> Just CREDIT_CARD
  "debit card" -> Just DEBIT_CARD
  "net banking" -> Just NETBANKING
  "wallet" -> Just WALLET
  "bank_transfer" -> Just BANK_TRANSFER
  "bharat qr" -> Just BHARAT_QR
  "international cards" -> Just INTERNATIONAL_CARD
  "cash cards" -> Just CASH_CARD
  "emi" -> Just EMI
  "buy now pay later" -> Just PAY_LATER
  _ -> Nothing

parseRazorpayDateTime :: Text -> Maybe UTCTime
parseRazorpayDateTime t
  | T.null (T.strip t) = Nothing
  | otherwise =
    parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack $ T.strip t)
      <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" (T.unpack $ T.strip t)
