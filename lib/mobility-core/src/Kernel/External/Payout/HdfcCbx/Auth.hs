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
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (fromEitherM)
import Kernel.Utils.Servant.Client
import Servant hiding (throwError)
import Web.FormUrlEncoded (ToForm (..))

data TokenReq = TokenReq
  { grantType :: Text,
    scope :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToForm TokenReq where
  toForm req = [("grant_type", req.grantType), ("scope", req.scope)]

data TokenResp = TokenResp
  { access_token :: Text,
    token_type :: Maybe Text,
    -- | Seconds. Callers should cache until shortly before this elapses; re-fetching per
    -- request works but wastes a round trip on every payout call.
    expires_in :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

type TokenAPI =
  "auth" :> "oauth" :> "v1" :> "token"
    :> BasicAuth "consumer-key-secret" BasicAuthData
    :> ReqBody '[FormUrlEncoded] TokenReq
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
      eulerClient = Euler.client proxy basic (TokenReq "client_credentials" scope)
  callAPI' (Just $ ManagerSelector tlsManagerKey) tokenUrl eulerClient "hdfc-oauth-token" proxy
    >>= fromEitherM (\err -> InternalError $ "HDFC CBX token request failed: " <> show err)
