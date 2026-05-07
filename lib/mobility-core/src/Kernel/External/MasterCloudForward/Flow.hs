{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}

module Kernel.External.MasterCloudForward.Flow
  ( HasMasterCloudForwarder (..),
    getRunApiInMasterCloud,
    runThroughMasterCloud,
  )
where

import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified EulerHS.Language as L
import EulerHS.Prelude (Free (..))
import qualified EulerHS.Types as ET
import Kernel.External.MasterCloudForward.API (forwardClient)
import Kernel.External.MasterCloudForward.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Utils.Logging
import Kernel.Utils.Servant.BaseUrl (showBaseUrlText)
import Kernel.Utils.Servant.Client (HasRequestId, defaultHttpManager)
import Network.HTTP.Media (MediaType, renderHeader)
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.URI as URI
import Servant.Client.Core
  ( ClientError (..),
    Request,
    RequestBody (..),
    Response,
    ResponseF (..),
    requestAccept,
    requestBody,
    requestHeaders,
    requestMethod,
    requestPath,
    requestQueryString,
  )
import qualified Servant.Client.Free as SCF
import System.Environment (lookupEnv)

-- | Reader-environment access for the unified 'MasterCloudProxyConfig'. The
-- same record is carried on every service's @AppEnv@; a side that doesn't
-- need a particular field just leaves it empty / 'Nothing'.
--
-- 'runThroughMasterCloud' reads @masterUrl@ and @masterSecret@ here; the
-- AWS-side handler in @Kernel.External.MasterCloudForward.Server@ reads
-- @masterSecret@ and @allowedHosts@.
class HasMasterCloudForwarder env where
  masterCloudProxyConfig :: env -> MasterCloudProxyConfig

-- | Reads the @RUN_API_IN_MASTER_CLOUD@ environment variable. Set to
-- @true@ on GCP pods that should tunnel external HTTP through the AWS
-- forwarder. Defaults to 'False' (direct call).
getRunApiInMasterCloud :: IO Bool
getRunApiInMasterCloud = do
  envVal <- lookupEnv "RUN_API_IN_MASTER_CLOUD"
  pure (fromMaybe False (readMaybe =<< envVal))

-- | Triple-gated wrapper around an @EulerClient@ call. If any of:
--
--   1. @RUN_API_IN_MASTER_CLOUD@ env var is @true@,
--   2. @masterCloudForwarderUrl@ is configured,
--   3. @masterCloudForwarderSecret@ is configured,
--
-- is missing, the call goes directly to the original 'BaseUrl' (identical
-- to a normal @callAPI'@). Otherwise, every @RunRequest@ emitted by the
-- @EulerClient@ free monad is intercepted, packaged into a 'ForwardRequest',
-- POSTed to the AWS forwarder, and the response is fed back to the
-- continuation so that the caller still gets back a typed @a@.
runThroughMasterCloud ::
  ( HasMasterCloudForwarder r,
    MonadReader r m,
    MonadFlow m,
    Log m,
    Metrics.CoreMetrics m,
    HasRequestId r
  ) =>
  -- | Original destination, e.g. Juspay's base URL
  BaseUrl ->
  -- | Typed Servant client value
  ET.EulerClient a ->
  -- | Description used in metrics/log lines
  Text ->
  m (Either ClientError a)
runThroughMasterCloud origBaseUrl eClient desc = do
  shouldForward <- liftIO getRunApiInMasterCloud
  cfg <- asks masterCloudProxyConfig
  case (shouldForward, cfg.masterUrl, cfg.masterSecret) of
    (True, Just fwdUrl, Just secret) -> do
      logDebug $ "MASTER_CLOUD_FORWARD: forwarding " <> desc <> " via " <> showBaseUrlText fwdUrl
      interpretWithForwarder origBaseUrl fwdUrl secret eClient desc
    _ -> do
      logDebug $ "MASTER_CLOUD_FORWARD: direct call for " <> desc
      L.callAPI' (Just defaultHttpManager) origBaseUrl eClient

-- | Walk the @EulerClient@ free monad: every 'SCF.RunRequest' is converted
-- into a 'ForwardRequest', shipped to the AWS forwarder, and its response is
-- decoded back into a Servant 'Response' fed to the continuation. Any
-- in-flight 'SCF.Throw' is surfaced as 'Left ClientError'.
interpretWithForwarder ::
  ( HasMasterCloudForwarder r,
    MonadReader r m,
    MonadFlow m,
    Log m,
    Metrics.CoreMetrics m,
    HasRequestId r
  ) =>
  BaseUrl ->
  BaseUrl ->
  Text ->
  ET.EulerClient a ->
  Text ->
  m (Either ClientError a)
interpretWithForwarder origBaseUrl fwdUrl secret (ET.EulerClient freeClient) desc =
  go freeClient
  where
    go (Pure a) = pure (Right a)
    go (Free (SCF.Throw e)) = pure (Left e)
    go (Free (SCF.RunRequest req cont)) =
      case mkForwardRequest origBaseUrl req of
        Left err -> pure (Left err)
        Right envelope -> do
          result <- callForwarder fwdUrl secret envelope desc
          case result of
            Left err -> pure (Left err)
            Right respEnvelope ->
              case mkServantResponse respEnvelope of
                Left err -> pure (Left err)
                Right response -> go (cont response)

-- | Convert a Servant @Request@ into the JSON 'ForwardRequest' envelope.
-- Streaming bodies are not supported here since they cannot be replayed at
-- the forwarder.
mkForwardRequest :: BaseUrl -> Request -> Either ClientError ForwardRequest
mkForwardRequest baseUrl req = do
  (bodyBytes, mbBodyMediaType) <- extractBody (requestBody req)
  let methodTxt = TE.decodeUtf8With ignoreErr (requestMethod req)
      pathBs = LBS.toStrict (BSB.toLazyByteString (requestPath req))
      queryBs = URI.renderQuery True (toList (requestQueryString req))
      basePart = TE.encodeUtf8 (showBaseUrlText baseUrl)
      fullUrlBs = basePart <> pathBs <> queryBs
      fullUrlTxt = TE.decodeUtf8With ignoreErr fullUrlBs
      explicitHdrs =
        toList $
          fmap
            ( \(name, value) ->
                ( TE.decodeUtf8With ignoreErr (CI.original name),
                  TE.decodeUtf8With ignoreErr value
                )
            )
            (requestHeaders req)
      contentTypeHdrs =
        case mbBodyMediaType of
          Just mt -> [("Content-Type", TE.decodeUtf8With ignoreErr (renderHeader mt))]
          Nothing -> []
      acceptList :: [MediaType]
      acceptList = toList (requestAccept req)
      acceptHdrs =
        case acceptList of
          [] -> []
          xs -> [("Accept", TE.decodeUtf8With ignoreErr (renderHeader xs))]
      hdrs = explicitHdrs <> contentTypeHdrs <> acceptHdrs
      bodyB64 = TE.decodeUtf8With ignoreErr (B64.encode bodyBytes)
  pure
    ForwardRequest
      { method = methodTxt,
        url = fullUrlTxt,
        headers = hdrs,
        body = bodyB64
      }
  where
    ignoreErr = TEE.lenientDecode

    extractBody :: Maybe (RequestBody, MediaType) -> Either ClientError (BS.ByteString, Maybe MediaType)
    extractBody Nothing = Right (BS.empty, Nothing)
    extractBody (Just (RequestBodyBS bs, mt)) = Right (bs, Just mt)
    extractBody (Just (RequestBodyLBS lbs, mt)) = Right (LBS.toStrict lbs, Just mt)
    extractBody (Just (RequestBodySource _, _)) =
      Left . ConnectionError . toException $
        userError "MASTER_CLOUD_FORWARD: streaming RequestBodySource is not supported by the forwarder"

-- | Convert the JSON 'ForwardResponse' envelope back into a Servant
-- 'Response' value the @EulerClient@ continuation can consume.
mkServantResponse :: ForwardResponse -> Either ClientError Response
mkServantResponse fr = do
  bodyBytes <-
    case B64.decode (TE.encodeUtf8 (fr.body)) of
      Right bs -> Right bs
      Left err ->
        Left . ConnectionError . toException $
          userError ("MASTER_CLOUD_FORWARD: failed to decode response body base64: " <> err)
  let statusObj = HTTP.mkStatus (fr.status) BS.empty
      hdrSeq =
        Seq.fromList $
          fmap
            ( \(n, v) ->
                ( CI.mk (TE.encodeUtf8 n),
                  TE.encodeUtf8 v
                )
            )
            (fr.headers)
  pure
    Response
      { responseStatusCode = statusObj,
        responseHeaders = hdrSeq,
        responseHttpVersion = HTTP.http11,
        responseBody = LBS.fromStrict bodyBytes
      }

-- | POST the envelope to the forwarder using the existing @callAPI'@
-- machinery so we get the same metrics, logging, and request-id propagation.
callForwarder ::
  ( MonadFlow m,
    Log m,
    Metrics.CoreMetrics m,
    MonadReader r m,
    HasRequestId r
  ) =>
  BaseUrl ->
  Text ->
  ForwardRequest ->
  Text ->
  m (Either ClientError ForwardResponse)
callForwarder fwdUrl secret envelope desc = do
  logDebug $
    "MASTER_CLOUD_FORWARD: calling forwarder for "
      <> desc
      <> " method="
      <> envelope.method
      <> " url="
      <> truncateUrl envelope.url
  L.callAPI'
    (Just defaultHttpManager)
    fwdUrl
    (forwardClient (Just secret) envelope)
  where
    truncateUrl u = if T.length u > 200 then T.take 200 u <> "..." else u
