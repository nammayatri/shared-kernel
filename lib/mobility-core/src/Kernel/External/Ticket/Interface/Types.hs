{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Ticket.Interface.Types
  ( module Reexport,
    module Kernel.External.Ticket.Interface.Types,
  )
where

import Deriving.Aeson
import Kernel.External.Ticket.Kapture.Config as Kapture
import Kernel.External.Ticket.Kapture.Types as Reexport (Classification (..), GetTicketReq (..), GetTicketResp (..), GetTicketStatusResp (..), KaptureCustomerReq (..), KaptureCustomerResp (..), KaptureEncryptionReq (..), KaptureEncryptionResp (..), KapturePullTicketReq (..), KapturePullTicketResp (..), PullAdditionalDetails (..), RideIdObject (..), SearchTicketByIdReq (..), TicketSummary (..), TicketType (..))
import Kernel.External.Ticket.Types as Reexport
import Kernel.External.Ticket.XyneSpaces.Config as XyneSpaces
import Kernel.External.Ticket.Zendesk.Config as Zendesk
import Kernel.Prelude
import Kernel.Types.Common (Money)
import Kernel.Types.HideSecrets

data IssueTicketServiceConfig
  = KaptureConfig Kapture.KaptureCfg
  | ZendeskConfig Zendesk.ZendeskCfg
  | XyneSpacesConfig XyneSpaces.XyneSpacesCfg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TicketStatus = Open | Pending | Solved | Closed | Reopened
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data CreateTicketResp = CreateTicketResp
  { ticketId :: Text,
    status :: TicketStatus,
    requesterId :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data UpdateTicketResp = UpdateTicketResp
  { ticketId :: Text,
    status :: TicketStatus,
    message :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data TicketContext
  = IssueTicket
  | SOSAlert
  | FeedbackTicket
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data CreateTicketReq = CreateTicketReq
  { category :: Text,
    subCategory :: Maybe Text,
    disposition :: Text,
    queue :: Text,
    issueId :: Maybe Text,
    issueDescription :: Text,
    mediaFiles :: Maybe [Text],
    name :: Maybe Text,
    phoneNo :: Maybe Text,
    personId :: Text,
    classification :: Classification,
    rideDescription :: Maybe RideInfo,
    becknIssueId :: Maybe Text,
    ticketContext :: Maybe TicketContext,
    xyneChannelId :: Maybe Text,
    -- | Xyne-only override for the FIRST chat message's body on the created
    -- thread. 'issueDescription' is shared across providers — Kapture's
    -- ticketDetails, Zendesk's rendered "Issue Description" section, and
    -- (when this is Nothing) Xyne's own message body all read it as the
    -- customer's actual description. When a merchant replays earlier
    -- bot/option history onto the Xyne thread (see IssueManagement's
    -- createIssueReport), the chronologically-first entry — which may be a
    -- bot message, not the customer's description — needs to become that
    -- first message instead, without disturbing what Kapture/Zendesk show.
    -- 'Nothing' preserves the original behaviour: falls back to
    -- 'issueDescription'.
    xyneTicketBody :: Maybe Text,
    -- | Xyne-only override for the sender name shown on that same first
    -- message. 'name' is shared too — Zendesk's requester name, Kapture's
    -- customerName, and Xyne's own "Customer Name" metadata field all read
    -- it as the real customer's name, so it must stay that on all of them
    -- even when 'xyneTicketBody' above is a bot-authored message. 'Nothing'
    -- falls back to 'name', same as today.
    xyneSenderName :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets CreateTicketReq where
  hideSecrets = identity

data RideInfo = RideInfo
  { rideShortId :: Text,
    rideCity :: Text,
    vehicleCategory :: Maybe Text,
    vehicleServiceTier :: Maybe Text,
    customerName :: Maybe Text,
    customerPhoneNo :: Maybe Text,
    driverName :: Maybe Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Text,
    status :: Text,
    rideCreatedAt :: UTCTime,
    pickupLocation :: Location,
    dropLocation :: Maybe Location,
    fare :: Maybe Money
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data Location = Location
  { lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data UpdateTicketReq = UpdateTicketReq
  { comment :: Text,
    ticketId :: Text,
    status :: TicketStatus,
    rideDescription :: Maybe RideInfo,
    issueDetails :: Maybe UpdateIssueDetails,
    requesterId :: Maybe Text,
    ticketContext :: Maybe TicketContext,
    name :: Maybe Text,
    phoneNo :: Maybe Text,
    xyneChannelId :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data UpdateIssueDetails = UpdateIssueDetails
  { issueDescription :: Maybe Text,
    issueId :: Maybe Text,
    mediaFiles :: Maybe [Text],
    subCategory :: Maybe Text,
    vehicleCategory :: Maybe Text,
    category :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | Status-only update payload. Used by 'updateTicketStatus' on ticket
-- providers that expose a dedicated status endpoint (XyneSpaces today).
-- @xyneTicketId@ is the opaque id returned by the provider on ticket create /
-- inbound append (for Xyne, the value in the @appDeskInbound@ response's
-- @ticketId@ field — NOT our internal @IssueReport.id@ / threadId).
data UpdateTicketStatusReq = UpdateTicketStatusReq
  { xyneTicketId :: Text,
    status :: TicketStatus,
    xyneChannelId :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | CSAT-only update payload. Used by 'updateTicketCsat' on ticket providers
-- that expose a dedicated CSAT endpoint (XyneSpaces today). @xyneTicketId@ is
-- the opaque id returned by the provider on ticket create / inbound append
-- (for Xyne, the value in the @appDeskInbound@ response's @ticketId@ field —
-- NOT our internal @IssueReport.id@ / threadId).
--
-- @rating@ is kept as free-form 'Text' rather than an enum: only one accepted
-- value ("GOOD") is confirmed from Xyne's docs so far, and the CSAT endpoint's
-- Zod schema rejects unrecognised fields hard — encoding an incomplete guess
-- as a closed sum type risks silently excluding valid values. Tighten to an
-- enum once the full accepted set is confirmed.
data UpdateTicketCsatReq = UpdateTicketCsatReq
  { xyneTicketId :: Text,
    rating :: Text,
    score :: Int,
    comment :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
