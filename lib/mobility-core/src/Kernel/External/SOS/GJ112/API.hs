{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SOS.GJ112.API where

import Kernel.External.SOS.GJ112.Types
import Kernel.Prelude
import Servant

-- | Authentication API
-- Endpoint: POST /api/authentication/no-auth/validateUser
type GJ112AuthAPI =
  "api"
    :> "authentication"
    :> "no-auth"
    :> "validateUser"
    :> ReqBody '[JSON] GJ112AuthReq
    :> Post '[JSON] GJ112AuthRes

-- | SOS Event Creation API
-- Endpoint: POST /api/citizen/saveSosPanicRequests
type GJ112SOSAPI =
  "api"
    :> "citizen"
    :> "saveSosPanicRequests"
    :> Header "Authorization" GJ112AuthToken
    :> ReqBody '[JSON] GJ112SOSReq
    :> Post '[JSON] GJ112SOSRes

gj112AuthAPI :: Proxy GJ112AuthAPI
gj112AuthAPI = Proxy

gj112SOSAPI :: Proxy GJ112SOSAPI
gj112SOSAPI = Proxy
