{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Moengage.Moengage.Flow
  ( pushEvent,
  )
where

import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE
import EulerHS.Types (client)
import Kernel.External.Encryption (EncFlow, decrypt)
import Kernel.External.Moengage.Moengage.API
import Kernel.External.Moengage.Moengage.Config
import Kernel.External.Moengage.Moengage.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

-- | Push an event to Moengage S2S API
pushEvent ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  MoengageCfg ->
  MoengageEventReq ->
  m MoengageEventResp
pushEvent cfg req = do
  unless cfg.enabled $
    throwError $ InternalError "Moengage integration is disabled"
  apiSecret <- decrypt cfg.apiSecret
  let authToken = buildBasicAuth cfg.appId apiSecret
      moengageClient = client (Proxy :: Proxy MoengageEventAPI)
  callAPI cfg.baseUrl (moengageClient cfg.appId (Just authToken) req) "moengageEvent" moengageEventAPI
    >>= fromEitherM (\err -> InternalError $ "Failed to call Moengage Event API: " <> show err)

buildBasicAuth :: Text -> Text -> Text
buildBasicAuth appId apiSecret =
  "Basic " <> TE.decodeUtf8 (B64.encode (TE.encodeUtf8 (appId <> ":" <> apiSecret)))
