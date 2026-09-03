{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | OAuth 2.0 client credentials for HDFC CBX.
--
-- This is the only call in the integration that is not JOSE-wrapped, and the only one that
-- uses HTTP Basic rather than a bearer token.
module Kernel.External.Payout.HdfcCbx.Auth (fetchToken, TokenResp (..)) where

import qualified Data.Text.Encoding as TE
import EulerHS.Types as Euler
import Kernel.External.Payout.HdfcCbx.Types.Payment (LenientInt)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (fromEitherM)
import Kernel.Utils.Servant.Client
import Servant hiding (throwError)
import Web.FormUrlEncoded (ToForm (..))

-- | The gateway wants @Content-Type: application/x-www-form-urlencoded@ present even
-- though the parameters travel in the query string; without it the token endpoint
-- answers @invalid_request: Missing or duplicate parameters@ (observed against UAT,
-- 2026-09-01). An empty form body is how servant is made to emit the header.
data EmptyForm = EmptyForm

instance ToForm EmptyForm where
  toForm _ = mempty

data TokenResp = TokenResp
  { access_token :: Text,
    token_type :: Maybe Text,
    -- | Seconds. Callers should cache until shortly before this elapses; re-fetching per
    -- request works but wastes a round trip on every payout call. UAT sends this as a
    -- JSON /string/ (@"899"@) -- same quirk as @nooftran@, hence 'LenientInt'.
    expires_in :: Maybe LenientInt
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | @grant_type@ and @scope@ travel in the query string with an empty body -- the shape
-- in HDFC's own Postman guide ("Params ...; Body: None"), confirmed working against UAT
-- on 2026-08-31. RFC 6749 prefers a form body, but that variant is unproven against this
-- gateway, so the proven shape wins.
type TokenAPI =
  "auth" :> "oauth" :> "v1" :> "token"
    :> BasicAuth "consumer-key-secret" BasicAuthData
    :> QueryParam' '[Required, Strict] "grant_type" Text
    :> QueryParam' '[Required, Strict] "scope" Text
    :> ReqBody '[FormUrlEncoded] EmptyForm
    :> Post '[JSON] TokenResp

-- | Exchange the consumer key and secret for a bearer token.
--
-- Uses the same mutually-authenticated manager as every other call: the client certificate
-- is required here too, so a token cannot be obtained from an unauthenticated host.
fetchToken ::
  (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  m TokenResp
fetchToken tlsManagerKey tokenUrl consumerKey consumerSecret scope = do
  let basic =
        BasicAuthData
          { basicAuthUsername = TE.encodeUtf8 consumerKey,
            basicAuthPassword = TE.encodeUtf8 consumerSecret
          }
      proxy = Proxy @TokenAPI
      eulerClient = Euler.client proxy basic "client_credentials" scope EmptyForm
  callAPI' (Just $ ManagerSelector tlsManagerKey) tokenUrl eulerClient "hdfc-oauth-token" proxy
    >>= fromEitherM (\err -> InternalError $ "HDFC CBX token request failed: " <> show err)
