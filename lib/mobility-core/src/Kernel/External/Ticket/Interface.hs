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
    updateTicketCsat,
    addAndUpdateKaptureCustomer,
    kaptureEncryption,
    kapturePullTicket,
    kaptureGetTicket,
    getTicketStatus,
    advanceBotConversation,
    endBotConversation,
  )
where

import qualified Kernel.External.Ticket.Interface.Kapture as Kapture
import Kernel.External.Ticket.Interface.Types
import qualified Kernel.External.Ticket.Interface.XyneSpaces as XyneSpaces
import qualified Kernel.External.Ticket.Interface.Zendesk as Zendesk
import qualified Kernel.External.Ticket.Kapture.Types as KT
import qualified Kernel.External.Ticket.XyneSpaces.Conversation as XC
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (throwError)
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
  RadarXyneSpacesConfig cfg -> XyneSpaces.createTicket cfg req

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
  RadarXyneSpacesConfig cfg -> XyneSpaces.updateTicket cfg req

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
  RadarXyneSpacesConfig cfg -> XyneSpaces.updateTicketStatus cfg req

-- | CSAT-only update. Only XyneSpaces has a dedicated endpoint; Kapture and
-- Zendesk are no-ops here.
updateTicketCsat ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IssueTicketServiceConfig ->
  UpdateTicketCsatReq ->
  m ()
updateTicketCsat serviceConfig req = case serviceConfig of
  KaptureConfig _ -> pure ()
  ZendeskConfig _ -> pure ()
  XyneSpacesConfig cfg -> XyneSpaces.updateTicketCsat cfg req
  RadarXyneSpacesConfig cfg -> XyneSpaces.updateTicketCsat cfg req

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
  RadarXyneSpacesConfig _ -> pure KT.KaptureCustomerResp {message = "Not applicable for RadarXyneSpaces", status = "ok", kaptureCustomerId = ""}

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
  RadarXyneSpacesConfig _ -> pure KT.KaptureEncryptionResp {success = True, encrytedCc = "", encryptedIv = ""}

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
  RadarXyneSpacesConfig _ -> pure KT.KapturePullTicketResp {totalCount = Just 0, message = [], status = "ok"}

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
  RadarXyneSpacesConfig _ -> pure []

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
  RadarXyneSpacesConfig _ -> pure []

-- | Pre-ticket support bot, one turn per call. Only XyneSpaces runs a bot
-- workflow; other providers have no equivalent.
advanceBotConversation ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IssueTicketServiceConfig ->
  XC.XyneConversationReq ->
  m XC.XyneConversationTurn
advanceBotConversation serviceConfig req = case serviceConfig of
  XyneSpacesConfig cfg -> XyneSpaces.advanceBotConversation cfg req
  RadarXyneSpacesConfig cfg -> XyneSpaces.advanceBotConversation cfg req
  KaptureConfig _ -> throwError $ InternalError "Bot conversation is only supported for XyneSpaces"
  ZendeskConfig _ -> throwError $ InternalError "Bot conversation is only supported for XyneSpaces"

endBotConversation ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IssueTicketServiceConfig ->
  Text ->
  m ()
endBotConversation serviceConfig conversationId = case serviceConfig of
  XyneSpacesConfig cfg -> XyneSpaces.endBotConversation cfg conversationId
  RadarXyneSpacesConfig cfg -> XyneSpaces.endBotConversation cfg conversationId
  KaptureConfig _ -> pure ()
  ZendeskConfig _ -> pure ()
