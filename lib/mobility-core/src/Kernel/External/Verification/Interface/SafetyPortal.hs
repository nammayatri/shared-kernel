{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Interface.SafetyPortal where

import Kernel.External.Encryption
import Kernel.External.Verification.Interface.Types
import qualified Kernel.External.Verification.SafetyPortal.Client as SafetyPortal
import Kernel.External.Verification.SafetyPortal.Config as Reexport
import Kernel.External.Verification.SafetyPortal.Types as Reexport
import qualified Kernel.External.Verification.SafetyPortal.Types as SafetyPortal
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Servant.Client

searchAgent ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  SafetyPortalCfg ->
  SafetyPortal.Agent ->
  m SearchAgentResponse
searchAgent cfg req = do
  let url = cfg.url
  token <- decrypt cfg.token
  let reqData =
        SafetyPortal.SearchAgentReq
          { suspectReqList = [req]
          }
  safetyPortalResp <- SafetyPortal.searchAgent url token reqData
  pure $ SearchAgentResponse {suspect = safetyPortalResp.suspects}
