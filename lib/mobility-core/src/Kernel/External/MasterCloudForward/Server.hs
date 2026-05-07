{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}

-- | AWS-side handler for the egress forwarder. Drop this handler under a
-- Servant route on whichever service runs in the master cloud (typically
-- driver-app on AWS). It validates the shared-secret header, validates the
-- destination URL against the allowlist, then re-issues the request from this
-- service's egress IP and returns the upstream response in a 'ForwardResponse'
-- envelope.
module Kernel.External.MasterCloudForward.Server
  ( forwardEgressHandler,
  )
where

import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified EulerHS.Language as L
import Kernel.External.MasterCloudForward.Types
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Error.Throwing (fromMaybeM, throwError)
import Kernel.Utils.Logging (logError)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as HTTP

-- | Servant-style handler matching the @ForwardAPI@ signature
-- (@Maybe Text -> ForwardRequest -> m ForwardResponse@). Reads its expected
-- secret and allowlist from the unified 'MasterCloudProxyConfig' so AWS-side
-- services share the same record their GCP-side counterparts use. Throws
-- 'ForwardError' variants on validation failure; the standard
-- @HTTPException@ translation layer in each service turns those into proper
-- HTTP responses.
--
-- Driver-app (or any AWS-deployed service) imports this and binds it under
-- its internal API tree, e.g.:
--
-- @
--   handler :: Maybe Text -> ForwardRequest -> FlowHandler ForwardResponse
--   handler s r = withFlowHandlerAPI $ forwardEgressHandler cfg mgr s r
-- @
forwardEgressHandler ::
  (MonadFlow m, Log m) =>
  MasterCloudProxyConfig ->
  -- | Reusable 'Http.Manager' used to talk to upstream. Egresses from the
  -- AWS-whitelisted IP. Typically the service's default manager.
  Http.Manager ->
  -- | @X-Forwarder-Secret@ value
  Maybe Text ->
  -- | Forwarded request envelope
  ForwardRequest ->
  m ForwardResponse
forwardEgressHandler cfg mgr mbSecret req = do
  -- 1. Auth check. We require BOTH the server config to declare a secret AND
  --    the caller to supply a matching one. A missing server-side secret
  --    means the forwarder isn't actually configured here — fail closed.
  expectedSecret <-
    cfg.masterSecret & fromMaybeM ForwardAuthFailure
  case mbSecret of
    Nothing -> throwError ForwardAuthFailure
    Just s | s /= expectedSecret -> throwError ForwardAuthFailure
    _ -> pure ()

  -- 2. Parse destination URL and validate host against the allowlist. We reuse
  --    http-client's parser so we don't take an extra dep on @network-uri@.
  destReq0 <-
    parseHttpRequest (req.url)
      & fromMaybeM (ForwardInvalidUrl req.url)
  let destHost = TE.decodeUtf8With TEE.lenientDecode (Http.host destReq0)
  when (destHost `notElem` (cfg.allowedHosts :: [Text])) $
    throwError (ForwardAllowlistDenied destHost)

  -- 3. Decode the base64-encoded body bytes from the envelope.
  bodyBytes <-
    case B64.decode (TE.encodeUtf8 req.body) of
      Right bs -> pure bs
      Left err ->
        throwError (ForwardUpstreamFailure ("invalid base64 request body: " <> T.pack err))

  -- 4. Reassemble the http-client Request: keep parsed url/host/port, override
  --    method/body/headers from the envelope, drop hop-by-hop headers
  --    http-client recomputes itself.
  let envelopeHeaders =
        [ (CI.mk (TE.encodeUtf8 name), TE.encodeUtf8 value)
          | (name, value) <- req.headers,
            let lname = T.toLower name,
            lname /= "content-length",
            lname /= "host",
            lname /= "transfer-encoding"
        ]
      destReq =
        destReq0
          { Http.method = TE.encodeUtf8 req.method,
            Http.requestBody = Http.RequestBodyBS bodyBytes,
            Http.requestHeaders = envelopeHeaders
          }

  -- 5. Send and translate any low-level exception to ForwardUpstreamFailure
  --    so callers get a structured 502 instead of an opaque 500.
  resp <-
    L.runIO (try @_ @SomeException (Http.httpLbs destReq mgr)) >>= \case
      Right r -> pure r
      Left e -> do
        logError $ "MASTER_CLOUD_FORWARD: upstream call failed: " <> show e
        throwError (ForwardUpstreamFailure (T.pack (show e)))

  -- 6. Package the upstream response back into the envelope. Status / headers
  --    transit verbatim; body is base64-encoded so binary payloads survive.
  let respBodyBs = LBS.toStrict (Http.responseBody resp)
      respHeaders =
        [ (TE.decodeUtf8With TEE.lenientDecode (CI.original name), TE.decodeUtf8With TEE.lenientDecode value)
          | (name, value) <- Http.responseHeaders resp
        ]
  pure
    ForwardResponse
      { status = HTTP.statusCode (Http.responseStatus resp),
        headers = respHeaders,
        body = TE.decodeUtf8With TEE.lenientDecode (B64.encode respBodyBs)
      }

-- | Parse a URL Text into an http-client 'Http.Request'. Returns 'Nothing' on
-- malformed input. Reuses http-client's @parseRequest@ in the @Maybe@ instance
-- of @MonadThrow@ so we don't pull in @network-uri@ just for URL parsing.
parseHttpRequest :: Text -> Maybe Http.Request
parseHttpRequest url = Http.parseRequest (T.unpack url)
