{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.CCAvenue.Flow
  ( getSettlementDetails,
    getPayoutSummary,
    getConsolidateSettlementDetails,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified EulerHS.Language as L
import Kernel.External.Encryption (decrypt)
import Kernel.External.Settlement.CCAvenue.ApiTypes
import Kernel.External.Settlement.CCAvenue.Crypto (ccaDecrypt, ccaEncrypt)
import Kernel.External.Settlement.Types (CCAvenuePGConfig (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as HttpTLS

-- ---------------------------------------------------------------------------
-- API #22: getSettlementDetails (by reference_no)
-- ---------------------------------------------------------------------------

getSettlementDetails ::
  ( EncFlow m r,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m
  ) =>
  CCAvenuePGConfig ->
  Text ->
  m SettlementDetailsResp
getSettlementDetails config referenceNo = do
  let reqPayload = A.toJSON (SettlementDetailsReq referenceNo)
  respJson <- callCCAvenueApi config "getSettlementDetails" "1.2" reqPayload
  case A.eitherDecode (LBS.fromStrict $ TE.encodeUtf8 respJson) of
    Right resp -> pure resp
    Left err -> throwError $ InternalError $ "CCAvenue getSettlementDetails parse error: " <> T.pack err

-- ---------------------------------------------------------------------------
-- API #20: payoutSummary (by settlement_date in dd-mm-yyyy format)
-- ---------------------------------------------------------------------------

getPayoutSummary ::
  ( EncFlow m r,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m
  ) =>
  CCAvenuePGConfig ->
  Text ->
  m PayoutSummaryResp
getPayoutSummary config settlementDate = do
  let reqPayload = A.toJSON (PayoutSummaryReq settlementDate)
  respJson <- callCCAvenueApi config "payoutSummary" "1.1" reqPayload
  case A.eitherDecode (LBS.fromStrict $ TE.encodeUtf8 respJson) of
    Right resp -> pure resp
    Left err -> throwError $ InternalError $ "CCAvenue payoutSummary parse error: " <> T.pack err

-- ---------------------------------------------------------------------------
-- API #25: ConsolidateSettlementDetails (by order_no + reference_no)
-- ---------------------------------------------------------------------------

getConsolidateSettlementDetails ::
  ( EncFlow m r,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m
  ) =>
  CCAvenuePGConfig ->
  Text ->
  Text ->
  m ConsolidateSettlementResp
getConsolidateSettlementDetails config orderNo referenceNo = do
  let reqPayload = A.toJSON (ConsolidateSettlementReq orderNo referenceNo)
  respJson <- callCCAvenueApi config "ConsolidateSettlementDetail" "DEF" reqPayload
  case A.eitherDecode (LBS.fromStrict $ TE.encodeUtf8 respJson) of
    Right resp -> pure resp
    Left err -> throwError $ InternalError $ "CCAvenue ConsolidateSettlementDetails parse error: " <> T.pack err

-- ---------------------------------------------------------------------------
-- Generic CCAvenue API call
-- Encrypt -> form POST -> parse status -> decrypt
-- ---------------------------------------------------------------------------

callCCAvenueApi ::
  ( EncFlow m r,
    MonadFlow m,
    MonadReader r m
  ) =>
  CCAvenuePGConfig ->
  Text ->
  Text ->
  A.Value ->
  m Text
callCCAvenueApi config command version payload = do
  accessCodePlain <- decrypt config.accessCode
  workingKeyPlain <- decrypt config.workingKey
  let jsonText = decodeUtf8 $ LBS.toStrict $ A.encode payload
  encRequest <- case ccaEncrypt workingKeyPlain jsonText of
    Right enc -> pure enc
    Left err -> throwError $ InternalError $ "CCAvenue encrypt failed: " <> err
  let formBody =
        "enc_request=" <> encRequest
          <> "&access_code="
          <> accessCodePlain
          <> "&command="
          <> command
          <> "&request_type=JSON"
          <> "&response_type=JSON"
          <> "&version="
          <> version
  respText <- L.runIO $ callCCAvenueHttp config.baseUrl formBody
  let formParams = parseFormResponse respText
      status = fromMaybe "" $ lookup "status" formParams
      encResponse = fromMaybe "" $ lookup "enc_response" formParams
  if status == "0"
    then case ccaDecrypt workingKeyPlain encResponse of
      Right decrypted -> pure decrypted
      Left err -> throwError $ InternalError $ "CCAvenue decrypt failed: " <> err
    else throwError $ InternalError $ "CCAvenue API error (status=" <> status <> "): " <> encResponse

callCCAvenueHttp :: BaseUrl -> Text -> IO Text
callCCAvenueHttp baseUrl formBody = do
  let url = showBaseUrl baseUrl <> "/apis/servlet/DoWebTrans"
  manager <- HttpTLS.newTlsManager
  initialReq <- Http.parseRequest (T.unpack url)
  let req =
        initialReq
          { Http.method = "POST",
            Http.requestBody = Http.RequestBodyBS (TE.encodeUtf8 formBody),
            Http.requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
          }
  resp <- Http.httpLbs req manager
  pure $ decodeUtf8 (LBS.toStrict $ Http.responseBody resp)

parseFormResponse :: Text -> [(Text, Text)]
parseFormResponse = map splitParam . T.splitOn "&"
  where
    splitParam s = case T.breakOn "=" s of
      (k, v) -> (k, T.drop 1 v)
