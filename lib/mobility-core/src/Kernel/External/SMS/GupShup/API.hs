{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.GupShup.API where

import EulerHS.Prelude
import Kernel.External.SMS.GupShup.Types
import Kernel.Types.App
  ( MandatoryQueryParam,
  )
import Servant

type ServiceAPI =
  "GatewayAPI"
    :> "rest"
    :> MandatoryQueryParam "method" Text
    :> MandatoryQueryParam "v" Double
    :> MandatoryQueryParam "userid" Text
    :> MandatoryQueryParam "password" Text
    :> MandatoryQueryParam "send_to" Text
    :> MandatoryQueryParam "msg" Text
    :> MandatoryQueryParam "msg_type" Text
    :> MandatoryQueryParam "auth_scheme" Text
    :> MandatoryQueryParam "format" Text
    :> MandatoryQueryParam "principalEntityId" Text
    :> MandatoryQueryParam "dltTemplateId" Text
    :> MandatoryQueryParam "mask" Text
    :> Get '[JSON] SubmitSmsRes

gupShupConnectAPI :: Proxy ServiceAPI
gupShupConnectAPI = Proxy
