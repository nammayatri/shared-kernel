{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Ticket.XyneSpaces.Types
  ( module Kernel.External.Ticket.XyneSpaces.Types,
  )
where

import Data.Aeson
import qualified Data.Map.Strict as Map
import Kernel.Prelude

-- | Request body for @POST /api/apps/ticket/appDeskInbound@.
-- Reuse the same @threadId@ to append to the same ticket; a new value starts a
-- new ticket. @externalId@ is the per-message retry-safe dedup key.
--
-- @ToJSON@ is hand-written to guarantee the exact wire shape Xyne expects:
-- optional fields are omitted (never emitted as @null@ — Xyne's Zod schema
-- would reject that with @VALIDATION_ERROR@), and @additionalFormFields@ is
-- emitted as a plain nested object under its own key. Earlier generic
-- encoding via @genericToJSON@ silently dropped the map field in some
-- contexts; the manual writer removes that ambiguity.
data XyneInboundReq = XyneInboundReq
  { channelId :: Text,
    threadId :: Text,
    subject :: Text,
    body :: Text,
    externalId :: Maybe Text,
    senderName :: Maybe Text,
    senderEmail :: Maybe Text,
    -- | Free-form key/value pairs shown to the Xyne agent as a metadata
    -- side panel on the ticket. The interface layer moves everything that
    -- used to be baked into 'body' (category, ride info, customer/driver
    -- phone numbers, media URLs) into here so the agent-facing @body@ stays
    -- limited to the customer's own message. Empty / 'Nothing' is dropped
    -- from the JSON payload.
    additionalFormFields :: Maybe (Map.Map Text Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

instance ToJSON XyneInboundReq where
  toJSON XyneInboundReq {..} =
    object $
      [ "channelId" .= channelId,
        "threadId" .= threadId,
        "subject" .= subject,
        "body" .= body
      ]
        <> maybe [] (\v -> ["externalId" .= v]) externalId
        <> maybe [] (\v -> ["senderName" .= v]) senderName
        <> maybe [] (\v -> ["senderEmail" .= v]) senderEmail
        <> maybe [] (\m -> if Map.null m then [] else ["additionalFormFields" .= m]) additionalFormFields

-- | Response body for @POST /api/apps/ticket/appDeskInbound@.
-- Status is 201 when @isNew@ is True (new ticket), 200 when False (appended).
data XyneInboundResp = XyneInboundResp
  { ticketId :: Text,
    xyneId :: Text,
    conversationId :: Text,
    isNew :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Request body for @POST /api/apps/ticket/updateTicket@ — status-only
-- update against a Xyne ticket. @ticketId@ is Xyne's own opaque id (the
-- @ticketId@ returned from @appDeskInbound@, not our threadId).
data XyneUpdateTicketReq = XyneUpdateTicketReq
  { ticketId :: Text,
    channelId :: Text,
    stageName :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Request body for @POST /api/csat/external/:ticketId@ — submits a
-- customer satisfaction rating against a Xyne ticket. @ticketId@ travels in
-- the URL path (see 'Kernel.External.Ticket.XyneSpaces.Flow.updateCsatAPI'),
-- not in this body.
data XyneCsatReq = XyneCsatReq
  { rating :: Text,
    score :: Int,
    comment :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Webhook event delivered to our @/internal/xyne/webhook@ endpoint when an
-- agent replies inside Xyne Spaces. Only DESK_REPLY is currently emitted.
data XyneWebhookEvent = XyneWebhookEvent
  { eventType :: Text,
    timestamp :: UTCTime,
    payload :: XyneDeskReplyPayload
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON XyneWebhookEvent where
  parseJSON = withObject "XyneWebhookEvent" $ \v ->
    XyneWebhookEvent
      <$> v .: "eventType"
      <*> v .: "timestamp"
      <*> v .: "payload"

instance ToJSON XyneWebhookEvent where
  toJSON XyneWebhookEvent {..} =
    object
      [ "eventType" .= eventType,
        "timestamp" .= timestamp,
        "payload" .= payload
      ]

-- | The @payload@ subfield of a @DESK_REPLY@ webhook event.
-- @threadId@ is the value we sent on the originating inbound call (= our
-- IssueReport id), used to route the reply back.
data XyneDeskReplyPayload = XyneDeskReplyPayload
  { channelId :: Text,
    threadId :: Text,
    conversationId :: Text,
    ticketId :: Maybe Text,
    externalId :: Text,
    body :: Text,
    attachments :: Maybe [XyneAttachment],
    replierUserId :: Maybe Text,
    replierName :: Maybe Text,
    workspaceId :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data XyneAttachment = XyneAttachment
  { name :: Text,
    url :: Text,
    mimeType :: Maybe Text,
    size :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Closed vocabularies of the @list/search@ contract, used on the
-- REQUEST side only. Constructor names are the exact wire strings.
-- Response fields deliberately stay 'Text': if Xyne ever grows a new
-- value, an enum decode would fail the whole page, while 'Text' degrades
-- a single cell.
--
-- Note: @statusV2@ is Xyne's internal workflow state; the ticket state a
-- desk shows as \"Status\" (BACKLOG, IN PROGRESS, COMPLETED, NOT
-- REQUIRED) is the board stage, filtered via 'stageName'.
data XyneTicketStatus = TODO | STARTED | PAUSED | CANCELLED | COMPLETED
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

data XynePriority = LOW | MEDIUM | HIGH | CRITICAL
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

data XyneTicketType = BUG | REQUEST
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Nested @filters@ object for @POST /api/apps/ticket/list/search@.
-- Every filter is list-typed on the wire — a single value travels as a
-- one-element list. 'Nothing' and empty lists are both omitted from the
-- payload: Xyne's Zod schema rejects @null@, and an empty array would
-- filter everything out rather than nothing.
data XyneListFilters = XyneListFilters
  { statusV2 :: Maybe [XyneTicketStatus],
    priority :: Maybe [XynePriority],
    -- | 'Text', not an enum: board stages are desk-configurable
    -- (\"BACKLOG\", \"IN PROGRESS\", \"Triage\", …), not a fixed vocabulary.
    stageName :: Maybe [Text],
    ticketType :: Maybe [XyneTicketType],
    -- | Xyne user ids
    assignedTo :: Maybe [Text],
    createdBy :: Maybe [Text],
    userGroupId :: Maybe [Text],
    tags :: Maybe [Text],
    isArchived :: Maybe Bool,
    createdAfter :: Maybe UTCTime,
    createdBefore :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

instance ToJSON XyneListFilters where
  toJSON XyneListFilters {..} =
    object $
      maybe [] (\vs -> ["statusV2" .= vs | not (null vs)]) statusV2
        <> maybe [] (\vs -> ["priority" .= vs | not (null vs)]) priority
        <> maybe [] (\vs -> ["stageName" .= vs | not (null vs)]) stageName
        <> maybe [] (\vs -> ["ticketType" .= vs | not (null vs)]) ticketType
        <> maybe [] (\vs -> ["assignedTo" .= vs | not (null vs)]) assignedTo
        <> maybe [] (\vs -> ["createdBy" .= vs | not (null vs)]) createdBy
        <> maybe [] (\vs -> ["userGroupId" .= vs | not (null vs)]) userGroupId
        <> maybe [] (\vs -> ["tags" .= vs | not (null vs)]) tags
        <> maybe [] (\v -> ["isArchived" .= v]) isArchived
        <> maybe [] (\v -> ["createdAfter" .= v]) createdAfter
        <> maybe [] (\v -> ["createdBefore" .= v]) createdBefore

-- | All-'Nothing' filters — start here and set what you need.
emptyXyneListFilters :: XyneListFilters
emptyXyneListFilters = XyneListFilters Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Request body for @POST /api/apps/ticket/list/search@.
--
-- At least one scope — 'channelId', 'projectId' or 'boardIds' — is
-- mandatory; supplying several ANDs them together. @channelId@ scope
-- additionally requires the calling app to be a participant of that
-- channel; the other two are workspace-scoped. The install JWT must carry
-- the @tickets:read@ permission. Callers enforce the scope requirement
-- (the type cannot).
--
-- Pagination is cursor-based: pass a previous response's 'nextCursor'
-- verbatim. Live-verified against the real API even though the official
-- doc omits it. There is no offset, no total and no free-text search.
--
-- @ToJSON@ is hand-written for the same reason as 'XyneInboundReq':
-- absent optionals must be omitted, never @null@. An all-'Nothing'
-- 'filters' value is dropped entirely rather than sent as @{}@.
data XyneListTicketsReq = XyneListTicketsReq
  { channelId :: Maybe Text,
    projectId :: Maybe Text,
    boardIds :: Maybe [Text],
    senderEmail :: Maybe Text,
    senderName :: Maybe Text,
    filters :: Maybe XyneListFilters,
    -- | Desk custom-field equality filters, keyed by field id.
    customFields :: Maybe (Map.Map Text Text),
    includeCustomFields :: Maybe Bool,
    limit :: Int,
    -- | Opaque page cursor from a previous response's @nextCursor@.
    cursor :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

instance ToJSON XyneListTicketsReq where
  toJSON XyneListTicketsReq {..} =
    object $
      ["limit" .= limit]
        <> maybe [] (\v -> ["channelId" .= v]) channelId
        <> maybe [] (\v -> ["projectId" .= v]) projectId
        <> maybe [] (\vs -> ["boardIds" .= vs | not (null vs)]) boardIds
        <> maybe [] (\v -> ["senderEmail" .= v]) senderEmail
        <> maybe [] (\v -> ["senderName" .= v]) senderName
        <> ( case filters of
               Just fs | toJSON fs /= object [] -> ["filters" .= fs]
               _ -> []
           )
        <> maybe [] (\m -> if Map.null m then [] else ["customFields" .= m]) customFields
        <> maybe [] (\v -> ["includeCustomFields" .= v]) includeCustomFields
        <> maybe [] (\v -> ["cursor" .= v]) cursor

-- | Channel-scoped request with everything else off — the RADAR default.
mkXyneListTicketsReq :: Text -> Int -> XyneListTicketsReq
mkXyneListTicketsReq channelId limit =
  XyneListTicketsReq
    { channelId = Just channelId,
      projectId = Nothing,
      boardIds = Nothing,
      senderEmail = Nothing,
      senderName = Nothing,
      filters = Nothing,
      customFields = Nothing,
      includeCustomFields = Nothing,
      limit = limit,
      cursor = Nothing
    }

-- | One row of a @list/search@ response. Deliberately lenient: every field
-- is optional so an upstream shape drift degrades a column, not the page.
-- Timestamps parse as 'UTCTime' (Xyne emits ISO-8601), matching
-- 'XyneWebhookEvent'.
data XyneTicketSummaryItem = XyneTicketSummaryItem
  { -- | Xyne's internal id (cuid) — the key for detail/conversation calls.
    ticketId :: Maybe Text,
    -- | The human-facing display number (\"NY-…\").
    xyneId :: Maybe Text,
    title :: Maybe Text,
    statusV2 :: Maybe Text,
    stageName :: Maybe Text,
    priority :: Maybe Text,
    createdAt :: Maybe UTCTime,
    lastEmailAt :: Maybe UTCTime,
    conversationId :: Maybe Text,
    channelId :: Maybe Text,
    boardId :: Maybe Text,
    projectId :: Maybe Text,
    customFormData :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Response body for @POST /api/apps/ticket/list/search@. A missing
-- @hasMore@ reads as \"no more pages\" — treat 'Nothing' as 'False'.
data XyneListTicketsResp = XyneListTicketsResp
  { items :: [XyneTicketSummaryItem],
    hasMore :: Maybe Bool,
    nextCursor :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The @metadata@ subobject of a ticket detail. The reporter of a desk
-- ticket lives here (and only here — list rows carry no sender).
data XyneTicketMetadata = XyneTicketMetadata
  { reporterEmail :: Maybe Text,
    fromEmailAddress :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Response body for @GET /api/apps/ticket/{ticketId}@ (404 on unknown
-- id — see 'Kernel.External.Ticket.XyneSpaces.Flow.getTicketAPI'). Lenient
-- like 'XyneTicketSummaryItem'. @history@ entries are heterogeneous per
-- activity type, so they stay 'Value' and callers pick what they render.
data XyneTicketDetail = XyneTicketDetail
  { id :: Maybe Text,
    xyneId :: Maybe Text,
    title :: Maybe Text,
    description :: Maybe Text,
    statusV2 :: Maybe Text,
    stageName :: Maybe Text,
    priority :: Maybe Text,
    createdAt :: Maybe UTCTime,
    updatedAt :: Maybe UTCTime,
    lastEmailAt :: Maybe UTCTime,
    conversationId :: Maybe Text,
    emailCount :: Maybe Int,
    metadata :: Maybe XyneTicketMetadata,
    history :: Maybe [Value]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | An attachment on a conversation message. This is a different wire
-- shape from 'XyneAttachment' (webhook payloads): here the desk sends
-- @originalFilename@\/@mimetype@, and the file itself is fetched by id via
-- @GET /api/apps/files/download/{id}@ (which needs the app JWT).
data XyneConversationAttachment = XyneConversationAttachment
  { id :: Maybe Text,
    originalFilename :: Maybe Text,
    mimetype :: Maybe Text,
    size :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | One message of a ticket's conversation. @body@ is raw HTML straight
-- from email — consumers own sanitization. @externalThreadId@ on the first
-- message is the thread key needed to reply via 'XyneInboundReq'.
data XyneConversationMessage = XyneConversationMessage
  { id :: Maybe Text,
    from :: Maybe Text,
    createdAt :: Maybe UTCTime,
    subject :: Maybe Text,
    externalThreadId :: Maybe Text,
    body :: Maybe Text,
    attachments :: Maybe [XyneConversationAttachment]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Response body for @GET /api/apps/ticket/{ticketId}/conversation@.
data XyneConversationResp = XyneConversationResp
  { items :: [XyneConversationMessage],
    hasMore :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
