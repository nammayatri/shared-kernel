module Kernel.External.PartnerSdk.Aarokya.Flow where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import EulerHS.Types as Euler
import Kernel.External.PartnerSdk.Aarokya.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified Servant.Client.Core as SCC

type GenerateTokenAPI =
  "auth"
    :> "token"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] AarokyaTokenRequest
    :> Post '[JSON] AarokyaTokenResponse

generateToken ::
  (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) =>
  BaseUrl ->
  Text ->
  AarokyaTokenRequest ->
  m AarokyaTokenResponse
generateToken url basicToken request = do
  let proxy = Proxy @GenerateTokenAPI
      eulerClient = Euler.client proxy (Just ("Basic " <> basicToken)) request
  callAarokyaAPI url eulerClient "aarokya-generate-token" proxy

-- | The response is intentionally typed as a raw JSON 'A.Value': NammaYatri is a
-- pass-through proxy for the contributor token and must forward whatever Aarokya
-- returns verbatim, without imposing (and breaking on) a fixed response shape.
type GenerateContributorTokenAPI =
  "auth"
    :> "contributor_token"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] AarokyaContributorTokenRequest
    :> Post '[JSON] A.Value

generateContributorToken ::
  (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) =>
  BaseUrl ->
  Text ->
  AarokyaContributorTokenRequest ->
  m A.Value
generateContributorToken url basicToken request = do
  let proxy = Proxy @GenerateContributorTokenAPI
      eulerClient = Euler.client proxy (Just ("Basic " <> basicToken)) request
  result <- callAPI url eulerClient "aarokya-generate-contributor-token" proxy
  case result of
    Right val -> pure val
    -- Proxy semantics: forward Aarokya's response body verbatim even on a non-2xx
    -- (e.g. a 4xx with {"status":"DISABLED"} when the driver has no contributor
    -- token). We return that body to our own caller as a 200 — the only hard
    -- failure is when there is no response body at all (connection error).
    Left (SCC.FailureResponse _ resp) -> pure $ forwardAarokyaBody resp
    Left (SCC.DecodeFailure _ resp) -> pure $ forwardAarokyaBody resp
    Left (SCC.UnsupportedContentType _ resp) -> pure $ forwardAarokyaBody resp
    Left (SCC.InvalidContentTypeHeader resp) -> pure $ forwardAarokyaBody resp
    Left err@(SCC.ConnectionError _) ->
      throwError $ InternalError $ "Failed to call aarokya-generate-contributor-token API: " <> show err

-- | Turn an Aarokya HTTP response body into a JSON 'A.Value' to forward as-is.
-- If the body is valid JSON we forward it structurally; otherwise we wrap the
-- raw text so nothing is lost.
forwardAarokyaBody :: SCC.Response -> A.Value
forwardAarokyaBody resp =
  let body = SCC.responseBody resp
   in fromMaybe (A.String $ TE.decodeUtf8 $ BSL.toStrict body) (A.decode body)

callAarokyaAPI :: (MonadFlow m, HasRequestId r, MonadReader r m) => CallAPI' m r api res res
callAarokyaAPI url eulerClient description proxy = do
  callAPI url eulerClient description proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call " <> description <> " API: " <> show err)
