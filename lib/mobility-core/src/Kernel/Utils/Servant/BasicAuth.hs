{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Utils.Servant.BasicAuth () where

import Control.Lens (at, (.=), (.~), (?=))
import qualified Data.OpenApi as DS
import Data.Typeable (typeRep)
import EulerHS.Prelude hiding (fromList, (.~))
import GHC.Exts (fromList)
import GHC.TypeLits (KnownSymbol)
import Kernel.Prelude.OrphanInstances ()
import Servant hiding (ResponseHeader (..))
import qualified Servant.OpenApi as S
import qualified Servant.OpenApi.Internal as S

-- WARNING this instance do not check any auth actually, this is only plug for compiling
instance HasContextEntry '[] (BasicAuthCheck BasicAuthData) where
  getContextEntry :: Context '[] -> BasicAuthCheck BasicAuthData
  getContextEntry EmptyContext = BasicAuthCheck {unBasicAuthCheck = pure . Authorized} -- TODO test it

instance
  ( S.HasOpenApi api,
    KnownSymbol realm
  ) =>
  S.HasOpenApi (BasicAuth realm BasicAuthData :> api)
  where
  toOpenApi _ =
    S.toOpenApi (Proxy @api)
      & addSecurityRequirement methodName verificationDescription headerName
      & S.addDefaultResponse400 headerName
      & addResponse401
    where
      headerName = "Authorization"
      methodName = show $ typeRep (Proxy @realm)
      verificationDescription =
        "The basic authentication scheme is based on the model that the \
        \client must authenticate itself with a user-ID and a password for \
        \each realm."

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
