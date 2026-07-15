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
    updateTicketStatus,
    addAndUpdateKaptureCustomer,
    kaptureEncryption,
    kapturePullTicket,
    kaptureGetTicket,
    getTicketStatus,
  )
where

import qualified Kernel.External.Ticket.Interface.Kapture as Kapture
import Kernel.External.Ticket.Interface.Types
import qualified Kernel.External.Ticket.Interface.XyneSpaces as XyneSpaces
import qualified Kernel.External.Ticket.Interface.Zendesk as Zendesk
import qualified Kernel.External.Ticket.Kapture.Types as KT
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Servant.Client

createTicket ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IssueTicketServiceConfig ->
  CreateTicketReq ->
  m CreateTicketResp
createTicket serviceConfig req = case serviceConfig of
  KaptureConfig cfg -> Kapture.createTicket cfg req
  ZendeskConfig cfg -> Zendesk.createTicket cfg req
  XyneSpacesConfig cfg -> XyneSpaces.createTicket cfg req

updateTicket ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IssueTicketServiceConfig ->
  UpdateTicketReq ->
  m UpdateTicketResp
updateTicket serviceConfig req = case serviceConfig of
  KaptureConfig cfg -> Kapture.updateTicket cfg req
  ZendeskConfig cfg -> Zendesk.updateTicket cfg req
  XyneSpacesConfig cfg -> XyneSpaces.updateTicket cfg req

-- | Status-only update. Only XyneSpaces has a dedicated endpoint; Kapture and
-- Zendesk fold status into their @updateTicket@ so this is a no-op there.
updateTicketStatus ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IssueTicketServiceConfig ->
  UpdateTicketStatusReq ->
  m ()
updateTicketStatus serviceConfig req = case serviceConfig of
  KaptureConfig _ -> pure ()
  ZendeskConfig _ -> pure ()
  XyneSpacesConfig cfg -> XyneSpaces.updateTicketStatus cfg req

addAndUpdateKaptureCustomer ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IssueTicketServiceConfig ->
  KaptureCustomerReq ->
  m KT.KaptureCustomerResp
addAndUpdateKaptureCustomer serviceConfig req = case serviceConfig of
  KaptureConfig cfg -> Kapture.addAndUpdateKaptureCustomer cfg req
  ZendeskConfig _ -> pure KT.KaptureCustomerResp {message = "Not applicable for Zendesk", status = "ok", kaptureCustomerId = ""}
  XyneSpacesConfig _ -> pure KT.KaptureCustomerResp {message = "Not applicable for XyneSpaces", status = "ok", kaptureCustomerId = ""}

kaptureEncryption ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IssueTicketServiceConfig ->
  KaptureEncryptionReq ->
  m KT.KaptureEncryptionResp
kaptureEncryption serviceConfig req = case serviceConfig of
  KaptureConfig cfg -> Kapture.kaptureEncryption cfg req
  ZendeskConfig _ -> pure KT.KaptureEncryptionResp {success = True, encrytedCc = "", encryptedIv = ""}
  XyneSpacesConfig _ -> pure KT.KaptureEncryptionResp {success = True, encrytedCc = "", encryptedIv = ""}

kapturePullTicket ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IssueTicketServiceConfig ->
  KapturePullTicketReq ->
  m KT.KapturePullTicketResp
kapturePullTicket serviceConfig req = case serviceConfig of
  KaptureConfig cfg -> Kapture.kapturePullTicket cfg req
  ZendeskConfig _ -> pure KT.KapturePullTicketResp {totalCount = Just 0, message = [], status = "ok"}
  XyneSpacesConfig _ -> pure KT.KapturePullTicketResp {totalCount = Just 0, message = [], status = "ok"}

kaptureGetTicket ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IssueTicketServiceConfig ->
  GetTicketReq ->
  m [KT.GetTicketResp]
kaptureGetTicket serviceConfig req = case serviceConfig of
  KaptureConfig cfg -> Kapture.kaptureGetTicket cfg req
  ZendeskConfig _ -> pure []
  XyneSpacesConfig _ -> pure []

getTicketStatus ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IssueTicketServiceConfig ->
  SearchTicketByIdReq ->
  m [KT.GetTicketStatusResp]
getTicketStatus serviceConfig req = case serviceConfig of
  KaptureConfig cfg -> Kapture.getTicketStatus cfg req
  ZendeskConfig _ -> pure []
  XyneSpacesConfig _ -> pure []
