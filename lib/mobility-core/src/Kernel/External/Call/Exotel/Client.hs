{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Call.Exotel.Client where

import Data.Maybe
import qualified Data.Text.Encoding as DT
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.External.Call.Exotel.Config
import Kernel.External.Call.Exotel.Types
import Kernel.External.Encryption (decrypt)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

-- | Exotel API interface
type InitiateCallAPI a =
  "v1" :> "Accounts"
    :> Capture "ExotelAccountSID" ExotelAccountSID
    :> "Calls"
    :> "connect.json"
    :> BasicAuth "apikey-apitoken" BasicAuthData
    :> ReqBody '[FormUrlEncoded] (ExotelInitiateCallReq a)
    :> Post '[JSON] ExotelInitiateCallResp

type ExotelCallbackAPI a =
  "statusCallback"
    :> ReqBody '[JSON] (ExotelCallCallbackReq a)
    :> Post '[JSON] ExotelCallCallbackResp

initiateCall ::
  ( CoreMetrics m,
    EncFlow m r,
    MonadFlow m,
    ToJSON a,
    HasRequestId r,
    MonadReader r m
  ) =>
  ExotelCfg ->
  ExotelInitiateCallReq a ->
  m ExotelInitiateCallResp
initiateCall ExotelCfg {..} exoRequest = do
  apiKey_ <- decrypt apiKey
  apiToken_ <- decrypt apiToken
  withLogTag "Exotel" $ do
    let authData =
          BasicAuthData
            (DT.encodeUtf8 $ getExotelApiKey apiKey_)
            (DT.encodeUtf8 $ getExotelApiToken apiToken_)
    callExotelAPI
      exotelUrl
      (callExotel accountSID authData)
      "initiateCall"
      (Proxy :: Proxy (InitiateCallAPI a))
  where
    callExotel sid authData = ET.client (Proxy :: Proxy (InitiateCallAPI a)) sid authData exoRequest

callExotelAPI :: CallAPI m r api a
callExotelAPI =
  callApiUnwrappingApiError
    (identity @ExotelError)
    Nothing
    (Just "EXOTEL_NOT_AVAILABLE")
    Nothing
