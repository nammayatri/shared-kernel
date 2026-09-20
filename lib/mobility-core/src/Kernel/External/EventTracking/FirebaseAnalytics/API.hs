{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.EventTracking.FirebaseAnalytics.API
  ( MpCollectAPI,
    mpCollectAPI,
    MpDebugCollectAPI,
    mpDebugCollectAPI,
    MpEmptyResponse,
  )
where

import qualified Data.List.NonEmpty as NE
import Kernel.External.EventTracking.FirebaseAnalytics.Types
import Kernel.Prelude
import qualified Network.HTTP.Media as M
import Servant

-- | @/mp/collect@ answers with an empty body and no useful content type. A
-- dedicated Accept-tagged type keeps that out of Servant's generic JSON
-- instances, as 'Kernel.Types.Servant.RawJson' does for raw bodies, and avoids
-- needing a 'ToJSON' instance for Servant's own 'NoContent'.
data MpEmptyResponse deriving (Typeable)

instance Accept MpEmptyResponse where
  contentTypes _ = NE.fromList ["application" M.// "json", "*" M.// "*"]

instance MimeUnrender MpEmptyResponse MpCollectResp where
  mimeUnrender _ _ = Right MpCollectResp

type MpCollectAPI =
  "mp" :> "collect"
    :> QueryParam' '[Required, Strict] "firebase_app_id" Text
    :> QueryParam' '[Required, Strict] "api_secret" Text
    :> ReqBody '[JSON] MpCollectReq
    :> Post '[MpEmptyResponse] MpCollectResp

mpCollectAPI :: Proxy MpCollectAPI
mpCollectAPI = Proxy

type MpDebugCollectAPI =
  "debug" :> "mp" :> "collect"
    :> QueryParam' '[Required, Strict] "firebase_app_id" Text
    :> QueryParam' '[Required, Strict] "api_secret" Text
    :> ReqBody '[JSON] MpCollectReq
    :> Post '[JSON] MpValidationResp

mpDebugCollectAPI :: Proxy MpDebugCollectAPI
mpDebugCollectAPI = Proxy
