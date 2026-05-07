{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.MasterCloudForward.API where

import qualified EulerHS.Types as ET
import Kernel.External.MasterCloudForward.Types
import Kernel.Prelude
import Servant

-- | Servant API exposed by the AWS-side egress forwarder.
-- The forwarder accepts a JSON envelope describing an outbound HTTP request,
-- re-issues it from the AWS egress IP, and returns the upstream response in a
-- JSON envelope.
type ForwardAPI =
  "forward-egress"
    :> Header "X-Forwarder-Secret" Text
    :> ReqBody '[JSON] ForwardRequest
    :> Post '[JSON] ForwardResponse

forwardAPI :: Proxy ForwardAPI
forwardAPI = Proxy

-- | Servant client for the forwarder. The first argument is the shared secret
-- bearer-style token used for authenticating GCP -> AWS forwarder hops.
forwardClient :: Maybe Text -> ForwardRequest -> ET.EulerClient ForwardResponse
forwardClient = ET.client forwardAPI
