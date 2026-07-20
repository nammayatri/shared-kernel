{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Meta.Flow where

import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.External.Encryption (decrypt)
import Kernel.External.Meta.Api
import Kernel.External.Meta.Config
import Kernel.External.Meta.Error (MetaError)
import Kernel.External.Meta.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error ()
import Kernel.Utils.Common

sendMessage ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  MetaCfg ->
  MetaSendMessageReq ->
  m MetaSendMessageResp
sendMessage cfg req = do
  token <- decrypt cfg.accessToken
  let authHeader = Just $ "Bearer " <> token
      eulerClient = ET.client (Proxy @MetaSendMessageAPI) cfg.apiVersion cfg.phoneNumberId authHeader req
  callMetaAPI cfg.baseUrl eulerClient "metaSendMessage" (Proxy @MetaSendMessageAPI)

-- Non-2xx -> fromResponse @MetaError on the failure body -> throws a typed
-- MetaError; transport errors -> ExternalAPICallError (Just "META_NOT_AVAILABLE").
-- `identity`, not `id` (RecordDotPreprocessor + Kernel.Prelude hide `id`).
callMetaAPI :: CallAPI m r api a
callMetaAPI =
  callApiUnwrappingApiError
    (identity @MetaError)
    Nothing
    (Just "META_NOT_AVAILABLE")
    Nothing
