{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.SafetyPortal.Client where

import qualified Data.Text as T
import qualified EulerHS.Types as ET
import Kernel.External.Verification.SafetyPortal.API as API
import Kernel.External.Verification.SafetyPortal.Config
import qualified Kernel.External.Verification.SafetyPortal.Error as SafetyPortal
import qualified Kernel.External.Verification.SafetyPortal.Types as SafetyPortal
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common
import Kernel.Utils.Common hiding (Error)

searchAgent ::
  ( MonadFlow m,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  SafetyPortal.SearchAgentReq ->
  m SafetyPortal.SearchAgentResp
searchAgent url apiKey req = do
  let client = API.searchAgent (Just apiKey) req
  callSafetyPortalAPI url client "searchAgent" API.searchAgentAPI

callSafetyPortalAPI :: CallAPI m r api res
callSafetyPortalAPI = callApiUnwrappingApiError (identity @SafetyPortal.SafetyPortalError) (Just $ ET.ManagerSelector $ T.pack safetyPortalHttpManagerKey) (Just "SAFETY_PORTAL_ERROR") Nothing
