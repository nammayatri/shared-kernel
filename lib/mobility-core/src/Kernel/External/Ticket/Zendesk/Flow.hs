{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Ticket.Zendesk.Flow
  ( createTicketAPI,
    updateTicketAPI,
  )
where

import EulerHS.Types as Euler
import qualified Kernel.External.Ticket.Zendesk.Types as Zendesk
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (fromEitherM)
import Kernel.Utils.Servant.Client
import Servant hiding (throwError)

type ZendeskCreateTicketAPI =
  "api"
    :> "v2"
    :> "tickets.json"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Zendesk.ZendeskCreateTicketReq
    :> Post '[JSON] Zendesk.ZendeskCreateTicketResp

type ZendeskUpdateTicketAPI =
  "api"
    :> "v2"
    :> "tickets"
    :> Capture "ticket_id" Text
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Zendesk.ZendeskUpdateTicketReq
    :> Put '[JSON] Zendesk.ZendeskUpdateTicketResp

createTicketAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  Zendesk.ZendeskCreateTicketReq ->
  m Zendesk.ZendeskCreateTicketResp
createTicketAPI url apiKey req = do
  let eulerClient = Euler.client (Proxy @ZendeskCreateTicketAPI)
  callAPI url (eulerClient (Just $ "Basic " <> apiKey) req) "zendeskCreateTicketAPI" (Proxy @ZendeskCreateTicketAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call Zendesk create ticket API: " <> show err)

updateTicketAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Zendesk.ZendeskUpdateTicketReq ->
  m Zendesk.ZendeskUpdateTicketResp
updateTicketAPI url apiKey ticketId req = do
  let eulerClient = Euler.client (Proxy @ZendeskUpdateTicketAPI)
  callAPI url (eulerClient ticketId (Just $ "Basic " <> apiKey) req) "zendeskUpdateTicketAPI" (Proxy @ZendeskUpdateTicketAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call Zendesk update ticket API: " <> show err)
