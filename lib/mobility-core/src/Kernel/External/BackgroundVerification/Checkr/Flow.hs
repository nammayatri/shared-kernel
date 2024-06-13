{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.BackgroundVerification.Checkr.Flow where

import qualified Data.Text.Encoding as DT
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.External.BackgroundVerification.Checkr.Config
import Kernel.External.BackgroundVerification.Checkr.Types
import Kernel.External.Encryption
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant

callCheckrAPI :: CallAPI' m api res res
callCheckrAPI url eulerClient description proxy = do
  callAPI url eulerClient description proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call " <> description <> " API: " <> show err)

mkBasicAuthData :: Text -> BasicAuthData
mkBasicAuthData apiKey =
  BasicAuthData
    { basicAuthUsername = DT.encodeUtf8 apiKey,
      basicAuthPassword = ""
    }

type CreateCandidateAPI =
  "v1" :> "candidates"
    :> BasicAuth "apikey-apitoken" BasicAuthData
    :> ReqBody '[FormUrlEncoded] CreateCandidateReq
    :> Post '[JSON] CreateCandidateResp

createCandidate ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    EncFlow m r
  ) =>
  CheckrCfg ->
  CreateCandidateReq ->
  m CreateCandidateResp
createCandidate cfg req = do
  let proxy = Proxy @CreateCandidateAPI
  apiKey <- decrypt cfg.apiKey
  let eulerClient = ET.client proxy (mkBasicAuthData apiKey) req
  callCheckrAPI cfg.url eulerClient "create-candidate" proxy

type CreateInvitationAPI =
  "v1" :> "invitations"
    :> BasicAuth "apikey-apitoken" BasicAuthData
    :> ReqBody '[FormUrlEncoded] CreateInvitationReq
    :> Post '[JSON] CreateInvitationResp

createCheckrInvitation ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    EncFlow m r
  ) =>
  CheckrCfg ->
  CreateInvitationReq ->
  m CreateInvitationResp
createCheckrInvitation cfg req = do
  let proxy = Proxy @CreateInvitationAPI
  apiKey <- decrypt cfg.apiKey
  let eulerClient = ET.client proxy (mkBasicAuthData apiKey) req
  callCheckrAPI cfg.url eulerClient "create-invitation" proxy

type GetInvitationAPI =
  "v1" :> "invitations"
    :> Capture "invitationId" Text
    :> BasicAuth "apikey-apitoken" BasicAuthData
    :> Get '[JSON] GetInvitationResp

getInvitationAPI :: Proxy GetInvitationAPI
getInvitationAPI = Proxy

getInvitation ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    EncFlow m r
  ) =>
  CheckrCfg ->
  Text ->
  m GetInvitationResp
getInvitation cfg invitationId = do
  let proxy = Proxy @GetInvitationAPI
  apiKey <- decrypt cfg.apiKey
  let eulerClient = ET.client proxy invitationId (mkBasicAuthData apiKey)
  callCheckrAPI cfg.url eulerClient "get-invitation" proxy

type GetReportAPI =
  "v1" :> "reports"
    :> Capture "reportId" Text
    :> BasicAuth "apikey-apitoken" BasicAuthData
    :> Get '[JSON] GetReportResp

getReport ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    EncFlow m r
  ) =>
  CheckrCfg ->
  Text ->
  m GetReportResp
getReport cfg reportId = do
  let proxy = Proxy @GetReportAPI
  apiKey <- decrypt cfg.apiKey
  let eulerClient = ET.client proxy reportId (mkBasicAuthData apiKey)
  callCheckrAPI cfg.url eulerClient "get-report" proxy
