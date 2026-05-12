{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Generic AWS-egress forwarder. Raw HTTP passthrough — no JSON envelope
-- on the wire. GCP-side @runThroughMasterCloud@ rewrites the request URL
-- and adds two headers; AWS-side @forwardEgressApp@ reads those headers
-- and replays the request from the whitelisted IP.
module Kernel.External.MasterCloudForward
  ( MasterCloudProxyConfig (..),
    emptyMasterCloudProxyConfig,
    HasMasterCloudForwarder (..),
    ForwardError (..),
    ForwardAPI,
    forwardAPI,
    runThroughMasterCloud,
    getRunApiInMasterCloud,
    forwardEgressApp,
  )
where

import qualified Control.Exception as Exc
import qualified Data.Aeson as A
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
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.Dhall (FromDhall)
import qualified Kernel.Utils.IOLogging as IOLog
import Kernel.Utils.Logging
import Kernel.Utils.Servant.BaseUrl (showBaseUrlText)
import Kernel.Utils.Servant.Client (HasRequestId, defaultHttpManager)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.URI as URI
import qualified Network.Wai as Wai
import Servant
import Servant.Client.Core (ClientError)
import qualified Servant.Client.Core as SCC
import qualified Servant.Client.Free as SCF
import qualified Servant.Server as SS
import System.Environment (lookupEnv)

-- Same record on both sides. GCP populates @masterUrl@ + @masterSecret@;
-- AWS populates @masterSecret@ only. Unused fields stay 'Nothing'.
data MasterCloudProxyConfig = MasterCloudProxyConfig
  { masterUrl :: Maybe BaseUrl,
    masterSecret :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, FromDhall)

emptyMasterCloudProxyConfig :: MasterCloudProxyConfig
emptyMasterCloudProxyConfig = MasterCloudProxyConfig Nothing Nothing

class HasMasterCloudForwarder env where
  masterCloudProxyConfig :: env -> MasterCloudProxyConfig

data ForwardError
  = ForwardNotConfigured
  | ForwardMissingSecret
  | ForwardBadSecret
  | ForwardMissingDestination
  | ForwardInvalidUrl Text
  | ForwardUpstreamFailure Text
  deriving (Eq, Generic, Show, IsBecknAPIError, FromJSON, ToJSON)

instanceExceptionWithParent 'HTTPException ''ForwardError

instance IsBaseError ForwardError where
  toMessage = \case
    ForwardNotConfigured -> Just "Forwarder secret not configured on this service."
    ForwardMissingSecret -> Just "Missing X-Forwarder-Secret header."
    ForwardBadSecret -> Just "X-Forwarder-Secret does not match."
    ForwardMissingDestination -> Just "Missing X-Forward-Destination header."
    ForwardInvalidUrl u -> Just $ "Invalid X-Forward-Destination URL: " <> u
    ForwardUpstreamFailure msg -> Just $ "Upstream forwarder failure: " <> msg

instance IsHTTPError ForwardError where
  toErrorCode = \case
    ForwardNotConfigured -> "FORWARDER_NOT_CONFIGURED"
    ForwardMissingSecret -> "FORWARDER_MISSING_SECRET"
    ForwardBadSecret -> "FORWARDER_BAD_SECRET"
    ForwardMissingDestination -> "FORWARDER_MISSING_DESTINATION"
    ForwardInvalidUrl _ -> "FORWARDER_INVALID_URL"
    ForwardUpstreamFailure _ -> "FORWARDER_UPSTREAM_FAILURE"

  toHttpCode = \case
    ForwardNotConfigured -> E401
    ForwardMissingSecret -> E401
    ForwardBadSecret -> E401
    ForwardMissingDestination -> E400
    ForwardInvalidUrl _ -> E400
    ForwardUpstreamFailure _ -> E500

instance IsAPIError ForwardError

type ForwardAPI =
  "forward-egress"
    :> Header "X-Forwarder-Secret" Text
    :> Header "X-Forward-Destination" Text
    :> Raw

forwardAPI :: Proxy ForwardAPI
forwardAPI = Proxy

-- True iff @RUN_API_IN_MASTER_CLOUD=True@ in env.
getRunApiInMasterCloud :: IO Bool
getRunApiInMasterCloud = do
  envVal <- lookupEnv "RUN_API_IN_MASTER_CLOUD"
  pure (fromMaybe False (readMaybe =<< envVal))

-- Drop-in replacement for @callAPI@. Triple-gated: env on + masterUrl set +
-- masterSecret set → forwarded. Anything else → direct call.
runThroughMasterCloud ::
  ( HasMasterCloudForwarder r,
    MonadReader r m,
    MonadFlow m,
    Log m,
    Metrics.CoreMetrics m,
    HasRequestId r
  ) =>
  BaseUrl ->
  ET.EulerClient a ->
  Text ->
  m (Either ClientError a)
runThroughMasterCloud origBaseUrl eClient desc = do
  shouldForward <- liftIO getRunApiInMasterCloud
  cfg <- asks masterCloudProxyConfig
  case (shouldForward, cfg.masterUrl, cfg.masterSecret) of
    (True, Just fwdUrl, Just secret) -> do
      logDebug $ "MASTER_CLOUD_FORWARD: forwarding " <> desc <> " via " <> showBaseUrlText fwdUrl
      interpretWithForwarder origBaseUrl fwdUrl secret eClient
    _ -> do
      logDebug $ "MASTER_CLOUD_FORWARD: direct call for " <> desc
      L.callAPI' (Just defaultHttpManager) origBaseUrl eClient

-- Walk the EulerClient free monad. Each RunRequest gets its path rewritten
-- to /forward-egress and two headers appended; the rewritten request is
-- dispatched against the AWS forwarder and the upstream response is fed
-- back to the continuation.
interpretWithForwarder ::
  ( MonadFlow m,
    Log m,
    Metrics.CoreMetrics m,
    MonadReader r m,
    HasRequestId r
  ) =>
  BaseUrl ->
  BaseUrl ->
  Text ->
  ET.EulerClient a ->
  m (Either ClientError a)
interpretWithForwarder origBaseUrl fwdUrl secret (ET.EulerClient freeClient) =
  go freeClient
  where
    go (Pure a) = pure (Right a)
    go (Free (SCF.Throw e)) = pure (Left e)
    go (Free (SCF.RunRequest req cont)) = do
      let pathBs = LBS.toStrict (BSB.toLazyByteString (SCC.requestPath req))
          queryBs = URI.renderQuery True (toList (SCC.requestQueryString req))
          basePart = TE.encodeUtf8 (showBaseUrlText origBaseUrl)
          originalFullUrl = TE.decodeUtf8With TEE.lenientDecode (basePart <> pathBs <> queryBs)
      let extraHeaders =
            SCC.requestHeaders req
              Seq.|> (CI.mk "X-Forwarder-Secret", TE.encodeUtf8 secret)
              Seq.|> (CI.mk "X-Forward-Destination", TE.encodeUtf8 originalFullUrl)
          r1 = req {SCC.requestPath = BSB.byteString "/forward-egress"}
          r2 = r1 {SCC.requestQueryString = mempty}
          newReq = r2 {SCC.requestHeaders = extraHeaders}
      let onestep = ET.EulerClient (Free (SCF.RunRequest newReq pure))
      result <- L.callAPI' (Just defaultHttpManager) fwdUrl onestep
      case result of
        Left err -> pure (Left err)
        Right response -> go (cont response)

-- AWS-side WAI handler. Validates secret, then replays the incoming request
-- to @X-Forward-Destination@ and proxies the response back. The 'LoggerEnv'
-- is the same one held on @AppEnv.loggerEnv@; log lines land in the standard
-- Kibana index alongside @logInfo@/@logError@ output from the rest of the app.
forwardEgressApp ::
  IOLog.LoggerEnv ->
  MasterCloudProxyConfig ->
  Http.Manager ->
  Wai.Application
forwardEgressApp logEnv cfg mgr req sendResp =
  case validateForwardRequest cfg req of
    Left err -> do
      IOLog.logOutputIO logEnv ERROR ("forward-egress validation failed: " <> T.pack (show err)) Nothing Nothing
      sendResp (forwardErrorResponse err)
    Right destReq -> proxyRequest logEnv destReq req sendResp mgr

-- Verify @X-Forwarder-Secret@ matches and parse @X-Forward-Destination@.
-- Returns the prepared upstream request, or a typed 'ForwardError'.
validateForwardRequest ::
  MasterCloudProxyConfig ->
  Wai.Request ->
  Either ForwardError Http.Request
validateForwardRequest cfg req = do
  let hdrs = Wai.requestHeaders req
      look n = TE.decodeUtf8With TEE.lenientDecode <$> lookup n hdrs
  expected <- cfg.masterSecret `orFail` ForwardNotConfigured
  got <- look "X-Forwarder-Secret" `orFail` ForwardMissingSecret
  when (got /= expected) $ Left ForwardBadSecret
  dest <- look "X-Forward-Destination" `orFail` ForwardMissingDestination
  Http.parseRequest (T.unpack dest) `orFail` ForwardInvalidUrl dest

orFail :: Maybe a -> e -> Either e a
orFail (Just a) _ = Right a
orFail Nothing e = Left e

-- Render a typed 'ForwardError' as a JSON @{ errorCode, errorMessage }@
-- response with the status code from 'toHttpCode'. Same envelope as the rest
-- of the Kernel error machinery.
forwardErrorResponse :: ForwardError -> Wai.Response
forwardErrorResponse err =
  Wai.responseLBS
    (httpCodeToStatus (toHttpCode err))
    [(CI.mk "Content-Type", "application/json")]
    ( A.encode $
        A.object
          [ "errorCode" A..= toErrorCode err,
            "errorMessage" A..= fromMaybe "" (toMessage err)
          ]
    )

-- 'HttpCode' lives in the Kernel error stack; @http-types@ uses 'HTTP.Status'.
-- 'toServerError' bridges them via Servant's @ServerError@ which carries an
-- 'Int' code.
httpCodeToStatus :: HttpCode -> HTTP.Status
httpCodeToStatus c =
  let se = toServerError c
   in HTTP.mkStatus (SS.errHTTPCode se) (TE.encodeUtf8 (T.pack (SS.errReasonPhrase se)))

proxyRequest ::
  IOLog.LoggerEnv ->
  Http.Request ->
  Wai.Request ->
  (Wai.Response -> IO Wai.ResponseReceived) ->
  Http.Manager ->
  IO Wai.ResponseReceived
proxyRequest logEnv destReq0 incoming sendResp mgr = do
  bodyBytes <- Wai.strictRequestBody incoming
  let destReq = buildDestRequest destReq0 incoming bodyBytes
  result <- Exc.try @Exc.SomeException (Http.httpLbs destReq mgr)
  case result of
    Left e -> do
      let decode = TE.decodeUtf8With TEE.lenientDecode
          destUrl =
            fromMaybe (decode (Http.host destReq <> Http.path destReq <> Http.queryString destReq)) $
              decode <$> lookup "X-Forward-Destination" (Wai.requestHeaders incoming)
      IOLog.logOutputIO
        logEnv
        ERROR
        ("forward-egress upstream FAILED " <> destUrl <> " err=" <> T.pack (show e))
        Nothing
        Nothing
    Right _ -> pure ()
  sendResp (toWaiResponse result)

-- Copy method/body/headers from the incoming WAI request onto the parsed
-- destination http-client request, dropping hop-by-hop headers and the two
-- forwarder-internal headers.
buildDestRequest :: Http.Request -> Wai.Request -> LBS.ByteString -> Http.Request
buildDestRequest base incoming body =
  let h = filter notHopByHop (Wai.requestHeaders incoming)
      r1 = base {Http.method = Wai.requestMethod incoming}
      r2 = r1 {Http.requestBody = Http.RequestBodyLBS body}
   in r2 {Http.requestHeaders = h}
  where
    notHopByHop (n, _) =
      n /= "Host"
        && n /= "Content-Length"
        && n /= "X-Forwarder-Secret"
        && n /= "X-Forward-Destination"

-- Translate an http-client outcome into a WAI response. http-client.httpLbs
-- auto-decompresses gzip, so we strip Content-Encoding / Content-Length /
-- Transfer-Encoding from the upstream headers to keep the body consistent.
toWaiResponse :: Either Exc.SomeException (Http.Response LBS.ByteString) -> Wai.Response
toWaiResponse = \case
  Left e ->
    forwardErrorResponse (ForwardUpstreamFailure (T.pack (show e)))
  Right resp ->
    let safeHeaders = filter (not . isStrippable . fst) (Http.responseHeaders resp)
     in Wai.responseLBS (Http.responseStatus resp) safeHeaders (Http.responseBody resp)
  where
    isStrippable n = n == "Content-Encoding" || n == "Content-Length" || n == "Transfer-Encoding"
