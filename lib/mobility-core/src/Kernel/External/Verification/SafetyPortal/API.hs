{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.SafetyPortal.API where

import qualified EulerHS.Types as T
import qualified Kernel.External.Verification.SafetyPortal.Types as SafetyPortal
import Kernel.Prelude
import Servant (Header, JSON, Post, ReqBody, (:>))

type SearchAgentAPI =
  "partner"
    :> "search"
    :> "agent"
    :> Header "token" Text
    :> ReqBody '[JSON] SafetyPortal.SearchAgentReq
    :> Post '[JSON] SafetyPortal.SearchAgentResp

searchAgentAPI :: Proxy SearchAgentAPI
searchAgentAPI = Proxy

searchAgent ::
  Maybe Text ->
  SafetyPortal.SearchAgentReq ->
  T.EulerClient SafetyPortal.SearchAgentResp
searchAgent = T.client searchAgentAPI
