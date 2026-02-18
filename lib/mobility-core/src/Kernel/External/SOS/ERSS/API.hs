{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SOS.ERSS.API
  ( ERSSPasswordAuthAPI,
    ERSSRefreshAuthAPI,
    ERSSInitialSOSAPI,
    ERSSTraceAPI,
    erssPasswordAuthAPI,
    erssRefreshAuthAPI,
    erssInitialSOSAPI,
    erssTraceAPI,
    erssMediaUploadAPI
  )
where

import Kernel.External.SOS.ERSS.Types
import Kernel.Prelude
import Kernel.ServantMultipart
import Servant

-- | Authentication API (Keycloak OpenID Connect)
-- Endpoint: /realms/ngerss/protocol/openid-connect/token
type ERSSPasswordAuthAPI =
  "realms"
    :> "ngerss"
    :> "protocol"
    :> "openid-connect"
    :> "token"
    :> ReqBody '[FormUrlEncoded] ERSSPasswordGrantReq
    :> Post '[JSON] ERSSAuthResponse

type ERSSRefreshAuthAPI =
  "realms"
    :> "ngerss"
    :> "protocol"
    :> "openid-connect"
    :> "token"
    :> ReqBody '[FormUrlEncoded] ERSSRefreshGrantReq
    :> Post '[JSON] ERSSAuthResponse

-- | Initial SOS Signal API
-- Endpoint: POST /public/api/sos/initial
type ERSSInitialSOSAPI =
  "public"
    :> "api"
    :> "sos"
    :> "initial"
    :> Header "Authorization" ERSSAuthToken
    :> ReqBody '[JSON] ERSSInitialSOSReq
    :> Post '[JSON] ERSSInitialSOSRes

-- | SOS Trace API for location updates
-- Endpoint: POST /public/api/sos/trace
type ERSSTraceAPI =
  "public"
    :> "api"
    :> "sos"
    :> "trace"
    :> Header "Authorization" ERSSAuthToken
    :> ReqBody '[JSON] ERSSTraceReq
    :> Post '[JSON] ERSSTraceRes

-- | Media Upload API
-- Endpoint: POST /erss/resource-manager/auth/upload/{auth-code}/{phone-number}/{file-name}
-- Content-Type: multipart/form-data
type ERSSMediaUploadAPI authCode phoneNumber fileName =
  "erss"
    :> "resource-manager"
    :> "auth"
    :> "upload"
    :> Capture "authCode" authCode
    :> Capture "phoneNumber" phoneNumber
    :> Capture "fileName" fileName
    :> Header "Authorization" ERSSAuthToken
    :> MultipartForm Tmp (MultipartData Tmp)
    :> Post '[JSON] ERSSMediaUploadRes

-- Proxy types for Servant client
erssPasswordAuthAPI :: Proxy ERSSPasswordAuthAPI
erssPasswordAuthAPI = Proxy

erssRefreshAuthAPI :: Proxy ERSSRefreshAuthAPI
erssRefreshAuthAPI = Proxy

erssInitialSOSAPI :: Proxy ERSSInitialSOSAPI
erssInitialSOSAPI = Proxy

erssTraceAPI :: Proxy ERSSTraceAPI
erssTraceAPI = Proxy

erssMediaUploadAPI :: Proxy (ERSSMediaUploadAPI Text Text Text)
erssMediaUploadAPI = Proxy
