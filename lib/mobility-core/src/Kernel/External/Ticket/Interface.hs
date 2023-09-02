{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Ticket.Interface
  ( createTicket,
    updateTicket,
  )
where

import qualified Kernel.External.Ticket.Interface.Kapture as Kapture
import Kernel.External.Ticket.Interface.Types
import qualified Kernel.External.Ticket.Kapture.Types as KT
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

createTicket ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  IssueTicketServiceConfig ->
  CreateTicketReq ->
  m KT.CreateTicketResp
createTicket serviceConfig req = case serviceConfig of
  KaptureConfig cfg -> Kapture.createTicket cfg req

updateTicket ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  IssueTicketServiceConfig ->
  UpdateTicketReq ->
  m KT.UpdateTicketResp
updateTicket serviceConfig req = case serviceConfig of
  KaptureConfig cfg -> Kapture.updateTicket cfg req
