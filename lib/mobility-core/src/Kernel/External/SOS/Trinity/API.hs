{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SOS.Trinity.API where

import Kernel.External.SOS.Trinity.Types
import Kernel.Prelude
import Servant

-- | Authentication API
-- Endpoint: POST /trinityPlatform/1.0.0/getAccessToken
type TrinityAuthAPI =
  "trinityPlatform"
    :> "1.0.0"
    :> "getAccessToken"
    :> ReqBody '[JSON] TrinityAuthReq
    :> Post '[JSON] TrinityAuthRes

-- | SOS Trigger API
-- Endpoint: POST /ngcadIntService/1.0.0/externalIntegration/triggerSOSMessage
type TrinitySOSAPI =
  "ngcadIntService"
    :> "1.0.0"
    :> "externalIntegration"
    :> "triggerSOSMessage"
    :> Header "Authorization" TrinityAuthToken
    :> ReqBody '[JSON] TrinitySOSReq
    :> Post '[JSON] TrinitySOSRes

trinityAuthAPI :: Proxy TrinityAuthAPI
trinityAuthAPI = Proxy

trinitySOSAPI :: Proxy TrinitySOSAPI
trinitySOSAPI = Proxy
