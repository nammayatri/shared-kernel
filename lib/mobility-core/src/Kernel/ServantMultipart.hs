{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kernel.ServantMultipart
  ( module Servant.Multipart,
    module Servant.Multipart.API,
    module Servant.Multipart.Client,
  )
where

import Control.Lens ((%~), (?~))
import qualified Control.Lens as L
import qualified Data.OpenApi as DS
import qualified Data.OpenApi.Declare as DD
import Kernel.Prelude
import Kernel.Utils.Monitoring.Prometheus.Servant
import Servant hiding (ResponseHeader (..))
import Servant.Multipart
import Servant.Multipart.API
import Servant.Multipart.Client
import qualified Servant.OpenApi as S

instance
  ( S.HasOpenApi api,
    DS.ToSchema a
  ) =>
  S.HasOpenApi (MultipartForm tag a :> api)
  where
  toOpenApi _ =
    S.toOpenApi (Proxy @api)
      & DS.components . DS.schemas %~ (<> defs)
      & DS.allOperations . DS.requestBody ?~ DS.Inline reqBody
    where
      (defs, formRef) = DD.runDeclare (DS.declareSchemaRef (Proxy @a)) mempty
      reqBody =
        (mempty :: DS.RequestBody)
          & DS.content . L.at "multipart/form-data"
            ?~ ((mempty :: DS.MediaTypeObject) & DS.schema ?~ formRef)
          & DS.required ?~ True

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (MultipartForm tag a :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)
