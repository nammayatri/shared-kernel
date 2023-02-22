 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Kernel.Utils.Servant.HeaderAuth where

import Control.Arrow
import Control.Lens ((?=))
import Data.List (lookup)
import qualified Data.OpenApi as DS
import Data.Typeable (typeRep)
import EulerHS.Prelude
import GHC.Exts (fromList)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Kernel.Tools.Metrics.CoreMetrics (HasCoreMetrics)
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (HasLog)
import Kernel.Utils.Servant.Server
import Network.Wai (Request (..))
import Servant hiding (ResponseHeader (..))
import Servant.Client
import qualified Servant.OpenApi as S
import qualified Servant.OpenApi.Internal as S
import Servant.Server.Internal.Delayed (addAuthCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, withRequest)

-- | Adds authentication via a header to API
--
-- Type argument defines what verification logic is supposed to do.
-- Normally you should define a type alias for this which fixes the
-- verification method.
data HeaderAuth (header :: Symbol) (verify :: Type)

-- | How token verification is performed.
class VerificationMethod verify where
  -- | Verification result, what is passed to the endpoint implementation.
  type VerificationResult verify

  -- | Description of this verification scheme as it appears in swagger.
  verificationDescription :: Text

-- | Implementation of verification.
data VerificationAction verify m = VerificationMethod verify =>
  VerificationAction
  { -- | Check given header value and extract the information which
    -- identifies the current user.
    -- This is allowed to fail with 'ServantError'.
    runVerifyMethod :: Text -> m (VerificationResult verify)
  }

-- | This server part implementation accepts token in @token@ header,
-- verifies it and puts @'VerificationResult'@ to your endpoint.
instance
  ( HasServer api ctx,
    HasEnvEntry r ctx,
    HasContextEntry ctx (VerificationAction verify (FlowR r)),
    VerificationMethod verify,
    KnownSymbol header,
    HasLog r,
    HasCoreMetrics r
  ) =>
  HasServer (HeaderAuth header verify :> api) ctx
  where
  type
    ServerT (HeaderAuth header verify :> api) m =
      VerificationResult verify -> ServerT api m

  route _ ctx subserver =
    route (Proxy @api) ctx $
      subserver `addAuthCheck` withRequest authCheck
    where
      authCheck :: Request -> DelayedIO (VerificationResult verify)
      authCheck req = runFlowRDelayedIO env . apiHandler $ do
        let headerName = fromString $ symbolVal (Proxy @header)
        requestHeaders req
          & (lookup headerName >>> fromMaybeM (MissingHeader headerName))
          >>= (parseHeader >>> fromEitherM (InvalidHeader headerName))
          >>= verifyMethod
      env = getEnvEntry ctx
      VerificationAction verifyMethod = getContextEntry ctx :: VerificationAction verify (FlowR r)

  hoistServerWithContext _ ctxp hst serv =
    hoistServerWithContext (Proxy @api) ctxp hst . serv

-- | This client part implementation simply accepts token and passes it to
-- the call.
instance
  (HasClient m api, KnownSymbol header) =>
  HasClient m (HeaderAuth header verify :> api)
  where
  type Client m (HeaderAuth header verify :> api) = RegToken -> Client m api

  clientWithRoute mp _ req =
    clientWithRoute
      mp
      (Proxy @(Header header Text :> api))
      req
      . Just

  hoistClientMonad mp _ hst cli = hoistClientMonad mp (Proxy @api) hst . cli

instance
  ( S.HasOpenApi api,
    VerificationMethod verify,
    Typeable verify,
    KnownSymbol header
  ) =>
  S.HasOpenApi (HeaderAuth header verify :> api)
  where
  toOpenApi _ =
    S.toOpenApi (Proxy @api)
      & addSecurityRequirement methodName (verificationDescription @verify) headerName
      & S.addDefaultResponse400 headerName
      & addResponse401
    where
      headerName = toText $ symbolVal (Proxy @header)
      methodName = show $ typeRep (Proxy @verify)

addSecurityRequirement :: Text -> Text -> Text -> DS.OpenApi -> DS.OpenApi
addSecurityRequirement methodName description headerName = execState $ do
  DS.components . DS.securitySchemes . at methodName ?= securityScheme
  DS.allOperations . DS.security .= one securityRequirement
  where
    securityScheme =
      DS.SecurityScheme
        { _securitySchemeDescription = Just description,
          _securitySchemeType =
            DS.SecuritySchemeApiKey
              DS.ApiKeyParams
                { _apiKeyName = headerName,
                  _apiKeyIn = DS.ApiKeyHeader
                }
        }
    securityRequirement =
      let scopes = []
       in DS.SecurityRequirement $ fromList [(methodName, scopes)]

addResponse401 :: DS.OpenApi -> DS.OpenApi
addResponse401 = execState $ do
  DS.components . DS.responses . at response401Name ?= response401
  DS.allOperations . DS.responses . DS.responses . at 401
    ?= DS.Ref (DS.Reference response401Name)
  where
    response401Name = "Unauthorized"
    response401 = mempty & DS.description .~ "Unauthorized"
