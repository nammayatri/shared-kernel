{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Ticket.Kapture.Flow
  ( createTicketAPI,
    updateTicketAPI,
  )
where

import EulerHS.Types as Euler
import qualified Kernel.External.Ticket.Kapture.Types as Kapture
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (callAPI, fromEitherM)
import Servant hiding (throwError)

type KaptureCreateTicketAPI =
  "add-ticket-from-other-source.html"
    :> Capture "version" Text
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Kapture.CreateTicketReq
    :> Post '[JSON] Kapture.CreateTicketResp

type KaputreUpdateTicketAPI =
  "update-ticket-from-other-source.html"
    :> Capture "version" Text
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Kapture.UpdateTicketReq
    :> Post '[JSON] Kapture.UpdateTicketResp

createTicketAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Kapture.CreateTicketReq ->
  m Kapture.CreateTicketResp
createTicketAPI url version auth req = do
  let eulerClient = Euler.client (Proxy @KaptureCreateTicketAPI)
  callAPI url (eulerClient version (Just auth) req) "createTicketAPI" (Proxy @KaptureCreateTicketAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call create ticket API: " <> show err)

updateTicketAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Kapture.UpdateTicketReq ->
  m Kapture.UpdateTicketResp
updateTicketAPI url version auth req = do
  let eulerClient = Euler.client (Proxy @KaputreUpdateTicketAPI)
  callAPI url (eulerClient version (Just auth) req) "updateTicketAPI" (Proxy @KaputreUpdateTicketAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call update ticket API: " <> show err)
