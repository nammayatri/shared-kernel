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
import qualified Data.Text.Encoding as DT
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
    :> BasicAuth "Authorization" BasicAuthData
    :> ReqBody '[FormUrlEncoded] (ClickToCallRequest)
    :> Post '[JSON] ClickToCallResponse

tataClickToCallAPI :: Proxy TataClickToCallAPI
tataClickToCallAPI = Proxy

tataInitiateCall ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r
  ) =>
  TataClickToCallCfg ->
  ClickToCallRequest ->
  m ClickToCallResponse
tataInitiateCall tataClickToCallCfg req = do
  apiKey_ <- decrypt tataClickToCallCfg.apiKey
  withLogTag "Tata ClickToCall" $ do
    let authData =
          BasicAuthData
            (DT.encodeUtf8 apiKey_)
            mempty
    callTataClickToCallAPI
      tataClickToCallCfg.url
      (callClickToCall authData req)
      "initiateTataClickToCall"
      tataClickToCallAPI
  where
    callClickToCall authData request = ET.client tataClickToCallAPI authData request

callTataClickToCallAPI :: CallAPI env api a
callTataClickToCallAPI =
  callApiUnwrappingApiError
    (identity @ClickToCallError)
    Nothing
    (Just "CLICKTOCALL_NOT_AVAILABLE")
    Nothing
