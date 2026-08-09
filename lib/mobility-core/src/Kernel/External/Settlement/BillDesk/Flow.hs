{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.BillDesk.Flow
  ( fetchBillDeskSettlementViaApi,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, parseTimeM)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified EulerHS.Language as L
import qualified EulerHS.Types as ET
import Kernel.External.Encryption (decrypt)
import Kernel.External.Settlement.BillDesk.Api (JoseResponse (..), retrieveSettlementAPI, retrieveSettlementClient, retrieveSettlementDetailsClient)
import Kernel.External.Settlement.BillDesk.ApiTypes
import Kernel.External.Settlement.BillDesk.BillDeskJose (createBillDeskRequest, parseBillDeskResponse)
import Kernel.External.Settlement.BillDesk.PaymentParser (parseBillDeskPaymentMethod)
import Kernel.External.Settlement.Interface.Types
import Kernel.External.Settlement.Types (BillDeskApiConfig (..))
import Kernel.External.Settlement.Utils.ParserUtils (parseAmount)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common

-- ---------------------------------------------------------------------------
-- Top-level entry point
-- ---------------------------------------------------------------------------

fetchBillDeskSettlementViaApi ::
  ( EncFlow m r,
    Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BillDeskApiConfig ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  m ParsePaymentSettlementResult
fetchBillDeskSettlementViaApi config fromDate toDate pvNumber = do
  clientIdPlain <- decrypt config.clientId
  signingKeyPlain <- decrypt config.signingKey
  encryptionKeyPlain <- decrypt config.encryptionKey
  encryptionKeyIdPlain <- decrypt config.encryptionKeyId
  let runtimeCfg =
        RuntimeBillDeskCfg
          { _baseUrl = config.baseUrl,
            _merchantId = config.merchantId,
            _clientId = clientIdPlain,
            _signingKey = TE.encodeUtf8 signingKeyPlain,
            _encryptionKey = TE.encodeUtf8 encryptionKeyPlain,
            _encryptionKeyId = encryptionKeyIdPlain
          }
  settlements <- retrieveSettlements runtimeCfg fromDate toDate pvNumber
  allRecords <- fmap concat . forM settlements $ \s ->
    retrieveAllSettlementDetails runtimeCfg s.pv_number
  let converted = zipWith (convertRecord settlements) [1 :: Int ..] allRecords
      (errs, goods) = partitionResults converted
  pure
    ParseResult
      { reports = goods,
        totalRows = length allRecords,
        failedRows = length errs,
        errors = errs
      }

-- ---------------------------------------------------------------------------
-- Runtime config (decrypted keys)
-- ---------------------------------------------------------------------------

data RuntimeBillDeskCfg = RuntimeBillDeskCfg
  { _baseUrl :: BaseUrl,
    _merchantId :: Text,
    _clientId :: Text,
    _signingKey :: BS.ByteString,
    _encryptionKey :: BS.ByteString,
    _encryptionKeyId :: Text
  }

-- ---------------------------------------------------------------------------
-- Retrieve Settlement
-- ---------------------------------------------------------------------------

retrieveSettlements ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  RuntimeBillDeskCfg ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  m [SettlementObj]
retrieveSettlements cfg fromDate toDate pvNumber = do
  let reqPayload =
        RetrieveSettlementReq
          { mercid = cfg._merchantId,
            from_date = fromDate,
            to_date = toDate,
            pv_number = pvNumber
          }
  respBytes <- callBillDeskApi cfg (A.toJSON reqPayload) retrieveSettlementClient
  case A.eitherDecode respBytes of
    Right settlements -> pure settlements
    Left err -> throwError $ InternalError $ "BillDesk Retrieve Settlement parse error: " <> T.pack err

-- ---------------------------------------------------------------------------
-- Retrieve Settlement Details (with pagination)
-- ---------------------------------------------------------------------------

retrieveAllSettlementDetails ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  RuntimeBillDeskCfg ->
  Text ->
  m [SettlementRecord]
retrieveAllSettlementDetails cfg pvNum = go 1 []
  where
    go pageNum acc = do
      let reqPayload =
            RetrieveSettlementDetailsReq
              { mercid = cfg._merchantId,
                pv_number = pvNum,
                page_number = pageNum
              }
      respBytes <- callBillDeskApi cfg (A.toJSON reqPayload) retrieveSettlementDetailsClient
      case A.eitherDecode respBytes of
        Left err -> throwError $ InternalError $ "BillDesk Settlement Details parse error: " <> T.pack err
        Right (detailsResp :: SettlementDetailsResp) ->
          let newAcc = acc <> detailsResp.records
              hasNext = maybe False (any (\l -> l.rel == "next")) detailsResp.links
           in if hasNext && pageNum < detailsResp.page_total
                then go (pageNum + 1) newAcc
                else pure newAcc

-- ---------------------------------------------------------------------------
-- Generic API call helper
-- Encrypt (JWE) -> Sign (JWS) -> Send -> Verify (JWS) -> Decrypt (JWE)
-- ---------------------------------------------------------------------------

callBillDeskApi ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  RuntimeBillDeskCfg ->
  A.Value ->
  ( Maybe Text ->
    Maybe Text ->
    LBS.ByteString ->
    ET.EulerClient JoseResponse
  ) ->
  m LBS.ByteString
callBillDeskApi cfg payload clientFn = do
  bdTraceId <- generateTraceId
  now <- getCurrentTime
  let epoch = round (utcTimeToPOSIXSeconds now) :: Integer
      timestamp = show epoch

  signedBody <-
    L.runIO (createBillDeskRequest cfg._clientId cfg._encryptionKey cfg._encryptionKeyId cfg._signingKey payload)
      >>= either (\e -> throwError $ InternalError $ "BillDesk request signing failed: " <> e) pure
  let bodyBytes = LBS.fromStrict (encodeUtf8 signedBody)

  let eulerClient =
        clientFn
          (Just bdTraceId)
          (Just timestamp)
          bodyBytes
  resp <-
    callAPI cfg._baseUrl eulerClient "billDeskSettlement" retrieveSettlementAPI
      >>= fromEitherM (ExternalAPICallError (Just "BILLDESK_SETTLEMENT_API_ERROR") cfg._baseUrl)

  let respText = decodeUtf8 (LBS.toStrict (unJoseResponse resp))
  case parseBillDeskResponse cfg._signingKey cfg._encryptionKey respText of
    Right decoded -> pure decoded
    Left err -> throwError $ InternalError $ "BillDesk response decode failed: " <> err

generateTraceId :: (MonadFlow m) => m Text
generateTraceId = do
  uuid <- L.runIO UUID.nextRandom
  pure $ T.filter (/= '-') (UUID.toText uuid)

-- ---------------------------------------------------------------------------
-- Record -> PaymentSettlementReport conversion
-- ---------------------------------------------------------------------------

convertRecord :: [SettlementObj] -> Int -> SettlementRecord -> Either Text PaymentSettlementReport
convertRecord settlements _idx rec =
  let rawJson = A.toJSON rec
      txnType' = parseTxnType (fromMaybe "" rec.transaction_type)
      settlementType' = case txnType' of
        ORDER -> Just CREDIT
        REFUND -> Just DEBIT
        CHARGEBACK -> Just DEBIT
      settlementUtr = listToMaybe settlements >>= (.utr)
   in Right
        PaymentSettlementReport
          { orderId = fromMaybe "" rec.merc_ref_id,
            txnId = rec.billdesk_id,
            rrn = rec.bank_ref_no,
            utr = settlementUtr,
            txnType = txnType',
            txnStatus = SUCCESS,
            txnDate = rec.date >>= parseDateText,
            txnAmount = parseAmountMaybe rec.amount,
            pgBaseFee = parseAmountMaybe rec.charges,
            pgTax = parseAmountMaybe rec.taxes,
            settlementAmount = parseAmountMaybe rec.net_amount,
            currency = INR,
            vendorId = Nothing,
            uniqueSplitId = Nothing,
            paymentGateway = Just "BILLDESK",
            paymentMethod = rec.payment_category >>= parseBillDeskPaymentMethod,
            paymentMethodSubType = Nothing,
            settlementType = settlementType',
            settlementMode = Nothing,
            settlementId = Nothing,
            settlementDate = rec.settlement_date >>= parseDateText,
            pgApprovalCode = rec.billdesk_id,
            pgRequestId = Nothing,
            bankId = rec.bankid,
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
            cardNetwork = rec.network,
            cardType = rec.card_type,
            isOffer = Nothing,
            offerCode = Nothing,
            offerId = Nothing,
            actualAmount = Nothing,
            cardNumber = Nothing
          }

parseTxnType :: Text -> TxnType
parseTxnType t = case T.toLower (T.strip t) of
  "refund" -> REFUND
  "chargeback" -> CHARGEBACK
  _ -> ORDER

parseDateText :: Text -> Maybe UTCTime
parseDateText t =
  parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t)
    <|> parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack t)

parseAmountMaybe :: Maybe Text -> HighPrecMoney
parseAmountMaybe = maybe 0 parseAmount

partitionResults :: [Either Text a] -> ([Text], [a])
partitionResults = foldr go ([], [])
  where
    go (Left e) (es, gs) = (e : es, gs)
    go (Right g) (es, gs) = (es, g : gs)
