{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Parser for the Juspay Portal transaction-export CSV
-- (@portal.juspay.in/api/q/download@ response).
--
-- Unlike the SFTP/Email settlement files produced by the PG's settlement pipeline,
-- this CSV is the portal's transaction dashboard export: it carries per-order
-- payment/refund state plus (when available) the settlement columns. Field set
-- and column ordering may vary between exports, so the parser looks columns up
-- by header name (via 'Csv.NamedRecord') rather than by position.
module Kernel.External.Settlement.JuspayApi.PaymentParser
  ( parseJuspayPortalCsv,
    parseJuspayPortalRow,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import Data.Either (partitionEithers)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Kernel.External.Settlement.Interface.Types
import Kernel.External.Settlement.Utils.ParserUtils (nonEmpty', parseAmount, parseDateTime)
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), HighPrecMoney)

parseJuspayPortalCsv :: LBS.ByteString -> ParsePaymentSettlementResult
parseJuspayPortalCsv csvData =
  case Csv.decodeByName csvData of
    Left err ->
      ParseResult
        { reports = [],
          totalRows = 0,
          failedRows = 0,
          errors = [T.pack $ "Juspay portal CSV parse error: " <> err]
        }
    Right (_hdr, rows) ->
      let rowList = V.toList (rows :: V.Vector Csv.NamedRecord)
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
      case parseJuspayPortalRow row of
        Left e -> Left $ "Row " <> show idx <> ": " <> e
        Right r -> Right r

-- | Lookup a header value. Returns @""@ when the column is absent so downstream
-- @nonEmpty'@ / @parseAmount@ / @parseDateTime@ helpers naturally short-circuit
-- to 'Nothing' / @0@ / 'Nothing' respectively.
look :: Csv.NamedRecord -> Text -> Text
look row name =
  case HM.lookup (TE.encodeUtf8 name) row of
    Just b -> TE.decodeUtf8 b
    Nothing -> ""

parseJuspayPortalRow :: Csv.NamedRecord -> Either Text PaymentSettlementReport
parseJuspayPortalRow row = do
  let orderIdT = T.strip (look row "Order Id")
  when (T.null orderIdT) $ Left "missing Order Id"
  let txnStatus' = mapPaymentStatus (look row "Payment Status")
      refundDate' = parseDateTime (look row "Refund Date")
      refundedAmt = parseAmount (look row "Order Amount Refunded")
      txnType'
        | isJust refundDate' && refundedAmt > 0 = REFUND
        | otherwise = ORDER
      -- Settlement Amount is the authoritative net-settled figure when present;
      -- fall back to Effective Txn Amount (post-offer, pre-fees) if the export
      -- was taken before settlement completed.
      settleAmount :: HighPrecMoney
      settleAmount =
        case readMaybe (T.unpack (T.strip (look row "Settlement Amount"))) of
          Just v -> v
          Nothing -> parseAmount (look row "Effective Txn Amount")
      offerAmt = parseAmount (look row "Offer Total Discount Amount")
      isOffer' = if offerAmt > 0 then Just True else Nothing
      currency' = case T.toUpper (T.strip (look row "Order Currency")) of
        "INR" -> INR
        _ -> INR
      rawJson =
        A.toJSON $
          HM.fromList
            [(TE.decodeUtf8 k, TE.decodeUtf8 v) | (k, v) <- HM.toList row]
  Right
    PaymentSettlementReport
      { orderId = orderIdT,
        txnId = nonEmpty' (look row "Txn Uuid") <|> nonEmpty' (look row "Juspay Txn Id"),
        rrn = padRrn <$> nonEmpty' (look row "Rrn"),
        utr = nonEmpty' (look row "Gateway Reference Id"),
        txnType = txnType',
        txnStatus = txnStatus',
        txnDate = parseDateTime (look row "Order Date Created") <|> parseDateTime (look row "System Date"),
        txnAmount = parseAmount (look row "Amount"),
        pgBaseFee = 0,
        pgTax = parseAmount (look row "Tax Amount"),
        settlementAmount = settleAmount,
        currency = currency',
        vendorId = nonEmpty' (look row "Sub Vendor Id"),
        uniqueSplitId = Nothing,
        paymentGateway = nonEmpty' (look row "Payment Gateway"),
        paymentMethod = parsePaymentMethod (look row "Payment Method Type"),
        paymentMethodSubType = nonEmpty' (look row "Payment Method Subtype"),
        settlementType = Just CREDIT,
        settlementMode = Nothing,
        settlementId = nonEmpty' (look row "Settlement Epg Id"),
        settlementDate = parseDateTime (look row "Execution Date"),
        pgApprovalCode = nonEmpty' (look row "Auth Code"),
        pgRequestId = nonEmpty' (look row "Epg Txn Id"),
        bankId = nonEmpty' (look row "Bank"),
        refundId = Nothing,
        refundArn = nonEmpty' (look row "ARN"),
        refundDate = refundDate',
        refundAmount = if txnType' == REFUND then Just refundedAmt else Nothing,
        refundBaseFee = Nothing,
        refundTax = Nothing,
        disputeId = Nothing,
        disputeType = Nothing,
        rawData = Just rawJson,
        cardIsin = nonEmpty' (look row "Card Bin"),
        cardNetwork = nonEmpty' (look row "Card Brand"),
        cardType = nonEmpty' (look row "Card Sub Type"),
        isOffer = isOffer',
        offerCode = nonEmpty' (look row "Merchant Offer Code"),
        offerId = nonEmpty' (look row "Offer Id's"),
        actualAmount = if offerAmt > 0 then Just (parseAmount (look row "Amount")) else Nothing,
        cardNumber = nonEmpty' (look row "Card Last Four Digits")
      }
  where
    padRrn r
      | T.length r < 12 = T.replicate (12 - T.length r) "0" <> r
      | otherwise = r

-- | Portal 'Payment Status' → internal 'TxnStatus'. Portal uses many terminal
-- states (@CHARGED@, @AUTHORIZED@, @AUTHORIZATION_FAILED@, ...); only the
-- money-moved states map to 'SUCCESS'.
mapPaymentStatus :: Text -> TxnStatus
mapPaymentStatus t = case T.toUpper (T.strip t) of
  "SUCCESS" -> SUCCESS
  "CHARGED" -> SUCCESS
  "AUTO_REFUNDED" -> SUCCESS
  _ -> FAILED

parsePaymentMethod :: Text -> Maybe PaymentMethodType
parsePaymentMethod t = case T.toUpper (T.strip t) of
  "UPI" -> Just UPI
  "UPI_INTENT" -> Just UPI
  "UPI_COLLECT" -> Just UPI
  "UPI_PAY" -> Just UPI
  "CREDIT_CARD" -> Just CREDIT_CARD
  "DEBIT_CARD" -> Just DEBIT_CARD
  "CARD" -> Just CREDIT_CARD
  "NB" -> Just NETBANKING
  "NETBANKING" -> Just NETBANKING
  "WALLET" -> Just WALLET
  "EMI" -> Just EMI
  "PAY_LATER" -> Just PAY_LATER
  _ -> Nothing
