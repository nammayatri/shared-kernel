 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kernel.ServantMultipart
  ( module Servant.Multipart,
  )
where

import Kernel.Prelude
import Kernel.Utils.Monitoring.Prometheus.Servant
import Servant hiding (ResponseHeader (..))
import Servant.Multipart
import qualified Servant.OpenApi as S

instance
  ( S.HasOpenApi api
  ) =>
  S.HasOpenApi (MultipartForm tag a :> api)
  where
  toOpenApi _ = S.toOpenApi (Proxy @api) -- TODO: implementing OpenAPI interpretation for Multipart.

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (MultipartForm tag a :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)
