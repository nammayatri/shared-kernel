{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.BillDesk.Flow
  ( fetchBillDeskSettlements,
    fetchBillDeskSettlementDetails,
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
import Kernel.External.Settlement.BillDesk.Api (JoseResponse (..), dynamicJoseAPI, dynamicJoseClient, retrieveSettlementAPI, retrieveSettlementClient, retrieveSettlementDetailsClient)
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
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import Servant.Client (ClientError (..), ResponseF (..))

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

fetchBillDeskSettlements ::
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
  m [SettlementObj]
fetchBillDeskSettlements config fromDate toDate pvNumber = do
  runtimeCfg <- buildRuntimeCfg config
  retrieveSettlements runtimeCfg fromDate toDate pvNumber

fetchBillDeskSettlementDetails ::
  ( EncFlow m r,
    Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BillDeskApiConfig ->
  SettlementObj ->
  m ParsePaymentSettlementResult
fetchBillDeskSettlementDetails config settlement = do
  runtimeCfg <- buildRuntimeCfg config
  allRecords <- retrieveAllSettlementDetails runtimeCfg settlement.pv_number
  let converted = zipWith (convertRecord settlement) [1 :: Int ..] allRecords
      (errs, goods) = partitionResults converted
  pure
    ParseResult
      { reports = goods,
        totalRows = length allRecords,
        failedRows = length errs,
        errors = errs
      }

buildRuntimeCfg ::
  ( EncFlow m r,
    MonadFlow m
  ) =>
  BillDeskApiConfig ->
  m RuntimeBillDeskCfg
buildRuntimeCfg config = do
  clientIdPlain <- decrypt config.clientId
  signingKeyPlain <- decrypt config.signingKey
  encryptionKeyPlain <- decrypt config.encryptionKey
  encryptionKeyIdPlain <- decrypt config.encryptionKeyId
  pure
    RuntimeBillDeskCfg
      { _baseUrl = config.baseUrl,
        _merchantId = config.merchantId,
        _clientId = clientIdPlain,
        _signingKey = TE.encodeUtf8 signingKeyPlain,
        _encryptionKey = TE.encodeUtf8 encryptionKeyPlain,
        _encryptionKeyId = encryptionKeyIdPlain
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
retrieveAllSettlementDetails cfg pvNum = go Nothing []
  where
    go maybeNextLink acc = do
      let reqPayload = case maybeNextLink of
            Nothing ->
              RetrieveSettlementDetailsReq
                { mercid = cfg._merchantId,
                  pv_number = pvNum,
                  page_number = 1
                }
            Just linkObj ->
              case linkObj.parameters of
                Just lp ->
                  RetrieveSettlementDetailsReq
                    { mercid = lp.mercid,
                      pv_number = lp.pv_number,
                      page_number = lp.page_number
                    }
                Nothing ->
                  RetrieveSettlementDetailsReq
                    { mercid = cfg._merchantId,
                      pv_number = pvNum,
                      page_number = 1
                    }
      respBytes <- case maybeNextLink >>= (.href) of
        Just href -> do
          baseUrl <- parseBaseUrl href
          callBillDeskApiWithUrl cfg baseUrl (A.toJSON reqPayload)
        Nothing ->
          callBillDeskApi cfg (A.toJSON reqPayload) retrieveSettlementDetailsClient
      case A.eitherDecode respBytes of
        Left err -> throwError $ InternalError $ "BillDesk Settlement Details parse error: " <> T.pack err
        Right (detailsResp :: SettlementDetailsResp) ->
          let newAcc = acc <> detailsResp.records
              nextLink = detailsResp.links >>= find (\l -> l.rel == "next")
           in case nextLink of
                Just nl -> go (Just nl) newAcc
                Nothing -> pure newAcc

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
callBillDeskApi cfg payload clientFn =
  sendBillDeskRequest cfg cfg._baseUrl payload clientFn "billDeskSettlement" retrieveSettlementAPI

callBillDeskApiWithUrl ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  RuntimeBillDeskCfg ->
  BaseUrl ->
  A.Value ->
  m LBS.ByteString
callBillDeskApiWithUrl cfg baseUrl payload =
  sendBillDeskRequest cfg baseUrl payload dynamicJoseClient "billDeskSettlementDetails" dynamicJoseAPI

sendBillDeskRequest ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m,
    SanitizedUrl api
  ) =>
  RuntimeBillDeskCfg ->
  BaseUrl ->
  A.Value ->
  ( Maybe Text ->
    Maybe Text ->
    LBS.ByteString ->
    ET.EulerClient JoseResponse
  ) ->
  Text ->
  Proxy api ->
  m LBS.ByteString
sendBillDeskRequest cfg baseUrl payload clientFn desc apiProxy = do
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
  result <- callAPI baseUrl eulerClient desc apiProxy
  case result of
    Left err -> handleClientError cfg err
    Right resp -> decryptAndCheckError cfg resp

handleClientError :: (MonadFlow m) => RuntimeBillDeskCfg -> ClientError -> m a
handleClientError cfg (FailureResponse _ (Response _statusCode _headers _httpVersion body)) = do
  let respText = decodeUtf8 (LBS.toStrict body)
  case parseBillDeskResponse cfg._signingKey cfg._encryptionKey respText of
    Right decoded ->
      case A.eitherDecode decoded of
        Right bdErr@BillDeskError {error_type = Just _} ->
          throwBillDeskError bdErr
        _ -> throwError $ InternalError $ "BillDesk API error: " <> decodeUtf8 (LBS.toStrict decoded)
    Left _ -> throwError $ InternalError $ "BillDesk API call failed: " <> respText
handleClientError _ err =
  throwError $ InternalError $ "BillDesk API call failed: " <> show err

-- | Decrypt the JWS/JWE response and check for BillDesk error.
decryptAndCheckError ::
  (MonadFlow m) =>
  RuntimeBillDeskCfg ->
  JoseResponse ->
  m LBS.ByteString
decryptAndCheckError cfg resp = do
  let respText = decodeUtf8 (LBS.toStrict (unJoseResponse resp))
  case parseBillDeskResponse cfg._signingKey cfg._encryptionKey respText of
    Left err -> throwError $ InternalError $ "BillDesk response decode failed: " <> err
    Right decoded ->
      case A.eitherDecode decoded of
        Right bdErr@BillDeskError {error_type = Just _} ->
          throwBillDeskError bdErr
        _ -> pure decoded

-- | Throw appropriate error based on BillDesk error response status code.
throwBillDeskError :: (MonadFlow m) => BillDeskError -> m a
throwBillDeskError bdErr =
  let errType = fromMaybe "unknown_error" bdErr.error_type
      errCode = fromMaybe "" bdErr.error_code
      msg = fromMaybe "" bdErr.message
      detail = "BillDesk " <> errType <> " [" <> errCode <> "]: " <> msg
   in case fromMaybe 500 bdErr.status of
        400 -> throwError $ InvalidRequest detail
        401 -> throwError $ Unauthorized
        403 -> throwError $ AccessDenied
        404 -> throwError $ InvalidRequest detail
        405 -> throwError $ InvalidRequest detail
        406 -> throwError $ InvalidRequest detail
        409 -> throwError $ InvalidRequest detail
        415 -> throwError $ InvalidRequest detail
        422 -> throwError $ InvalidRequest detail
        _ -> throwError $ InternalError detail

generateTraceId :: (MonadFlow m) => m Text
generateTraceId = do
  uuid <- L.runIO UUID.nextRandom
  pure $ T.filter (/= '-') (UUID.toText uuid)

-- ---------------------------------------------------------------------------
-- Record -> PaymentSettlementReport conversion
-- ---------------------------------------------------------------------------

convertRecord :: SettlementObj -> Int -> SettlementRecord -> Either Text PaymentSettlementReport
convertRecord settlement _idx rec =
  let txnType' = parseTxnType (fromMaybe "" rec.transaction_type)
      base = baseReport settlement rec txnType'
   in Right $ case txnType' of
        ORDER -> toSettlementReport base rec
        REFUND -> toRefundReport base rec
        CHARGEBACK -> toChargebackReport base rec
        REFUND_REVERSAL -> toRefundReversalReport base rec
        CHARGEBACK_REVERSAL -> toChargebackReversalReport base rec
        ADJUSTMENT -> toAdjustmentReport base rec

baseReport :: SettlementObj -> SettlementRecord -> TxnType -> PaymentSettlementReport
baseReport settlement rec txnType' =
  PaymentSettlementReport
    { orderId = "",
      txnId = Nothing,
      rrn = rec.bank_ref_no,
      utr = settlement.utr,
      txnType = txnType',
      txnStatus = SUCCESS,
      txnDate = rec.date >>= parseDateText,
      txnAmount = parseAmountMaybe rec.amount,
      pgBaseFee = 0,
      pgTax = 0,
      settlementAmount = 0,
      currency = INR,
      vendorId = Nothing,
      uniqueSplitId = Nothing,
      paymentGateway = Just "BILLDESK",
      paymentMethod = Nothing,
      paymentMethodSubType = Nothing,
      settlementType = Nothing,
      settlementMode = Nothing,
      settlementId = Nothing,
      settlementDate = Nothing,
      pgApprovalCode = Nothing,
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
      rawData = Just (A.toJSON rec),
      cardIsin = Nothing,
      cardNetwork = rec.network,
      cardType = rec.card_type,
      isOffer = Nothing,
      offerCode = Nothing,
      offerId = Nothing,
      actualAmount = Nothing,
      cardNumber = Nothing
    }

toSettlementReport :: PaymentSettlementReport -> SettlementRecord -> PaymentSettlementReport
toSettlementReport base rec =
  base
    { orderId = fromMaybe "" rec.merc_ref_id,
      txnId = rec.billdesk_id,
      pgBaseFee = parseAmountMaybe rec.charges,
      pgTax = parseAmountMaybe rec.taxes,
      settlementAmount = parseAmountMaybe rec.net_amount,
      actualAmount = Just (parseAmountMaybe rec.gross_amount),
      paymentMethod = rec.payment_category >>= parseBillDeskPaymentMethod,
      settlementType = Just CREDIT,
      settlementDate = rec.settlement_date >>= parseDateText,
      pgApprovalCode = rec.authcode
    }

toRefundReport :: PaymentSettlementReport -> SettlementRecord -> PaymentSettlementReport
toRefundReport base rec =
  base
    { orderId = fromMaybe "" rec.merc_ref_id,
      txnId = rec.reference_id,
      refundId = rec.billdesk_id,
      refundAmount = Just (parseAmountMaybe rec.amount),
      refundDate = rec.reference_date >>= parseDateText,
      actualAmount = Just (parseAmountMaybe rec.gross_amount),
      settlementType = Just DEBIT
    }

toChargebackReport :: PaymentSettlementReport -> SettlementRecord -> PaymentSettlementReport
toChargebackReport base rec =
  base
    { txnId = rec.reference_id,
      disputeId = rec.billdesk_id,
      disputeType = Just OTHER_DISPUTE,
      actualAmount = Just (parseAmountMaybe rec.gross_amount),
      refundDate = rec.reference_date >>= parseDateText,
      refundAmount = Just (parseAmountMaybe rec.reference_amount),
      settlementType = Just DEBIT
    }

toRefundReversalReport :: PaymentSettlementReport -> SettlementRecord -> PaymentSettlementReport
toRefundReversalReport base rec =
  base
    { txnId = rec.reference_id,
      refundId = rec.billdesk_id,
      refundAmount = Just (parseAmountMaybe rec.amount),
      refundDate = rec.reference_date >>= parseDateText,
      settlementType = Just CREDIT
    }

toChargebackReversalReport :: PaymentSettlementReport -> SettlementRecord -> PaymentSettlementReport
toChargebackReversalReport base rec =
  base
    { txnId = rec.reference_id,
      disputeId = rec.billdesk_id,
      disputeType = Just OTHER_DISPUTE,
      refundDate = rec.reference_date >>= parseDateText,
      refundAmount = Just (parseAmountMaybe rec.reference_amount),
      settlementType = Just CREDIT
    }

toAdjustmentReport :: PaymentSettlementReport -> SettlementRecord -> PaymentSettlementReport
toAdjustmentReport base rec =
  base
    { txnId = rec.billdesk_id,
      settlementAmount = parseAmountMaybe rec.amount
    }

parseTxnType :: Text -> TxnType
parseTxnType t = case T.toLower (T.strip t) of
  "settlement" -> ORDER
  "refund" -> REFUND
  "chargeback" -> CHARGEBACK
  "refundreversal" -> REFUND_REVERSAL
  "chargebackreversal" -> CHARGEBACK_REVERSAL
  "adjustment" -> ADJUSTMENT
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
