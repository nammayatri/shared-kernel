{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Call.TataClickToCall.Client where

import Data.Maybe
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.External.Call.TataClickToCall.Config
import Kernel.External.Call.TataClickToCall.Types
import Kernel.External.Encryption (decrypt)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

type TataClickToCallAPI =
  "app" :> "cpaas" :> "pv" :> "v1" :> "click_to_call"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] ClickToCallConnectRequest
    :> Post '[JSON] ClickToCallResponse

tataClickToCallAPI :: Proxy TataClickToCallAPI
tataClickToCallAPI = Proxy

tataInitiateCall ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  TataClickToCallCfg ->
  ClickToCallConnectRequest ->
  m ClickToCallResponse
tataInitiateCall tataClickToCallCfg req = do
  apiKey_ <- decrypt tataClickToCallCfg.apiKey
  logDebug $ "logging tataClickToCallCfg request_ " <> show req
  let eulerClient = ET.client (Proxy @TataClickToCallAPI)
  response <- callAPI tataClickToCallCfg.url (eulerClient (Just $ "Bearer " <> apiKey_) req) "tataInitiateCall" (Proxy @TataClickToCallAPI) >>= fromEitherM (ClickToCallGenericError . T.pack . show)
  when (response.success == False) (logError $ "logging tataClickToCallCfg response_ " <> show response)
  return response
