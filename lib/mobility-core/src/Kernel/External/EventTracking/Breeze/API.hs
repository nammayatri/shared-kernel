{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.EventTracking.Breeze.API
  ( BreezeEventAPI,
    breezeEventAPI,
  )
where

import qualified Data.Aeson as A
import Kernel.External.EventTracking.Breeze.Types
import Kernel.Prelude
import qualified Network.HTTP.Media as M
import Servant

-- | The collector takes newline-delimited JSON: one event per line, under
-- @Content-Type: application/json@. We send one event per request, so the body is
-- the event on a single line followed by a newline.
--
-- A dedicated 'Accept'-tagged type keeps this out of Servant's generic JSON
-- instances, which would not add the newline.
data BreezeNdJson

instance Accept BreezeNdJson where
  contentType _ = "application" M.// "json"

instance MimeRender BreezeNdJson BreezeEvent where
  mimeRender _ event = A.encode event <> "\n"

type BreezeEventAPI =
  "v1" :> "events" :> "authenticated"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-tenant-id" Text
    :> Header' '[Required, Strict] "x-workspace-id" Text
    :> ReqBody '[BreezeNdJson] BreezeEvent
    :> Post '[JSON] BreezeEventResp

breezeEventAPI :: Proxy BreezeEventAPI
breezeEventAPI = Proxy
