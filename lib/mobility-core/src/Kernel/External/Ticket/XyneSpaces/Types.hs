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
import Kernel.Prelude

-- | Request body for @POST /api/apps/ticket/appDeskInbound@.
-- Reuse the same @threadId@ to append to the same ticket; a new value starts a
-- new ticket. @externalId@ is the per-message retry-safe dedup key.
--
-- @ToJSON@ omits @Nothing@ fields rather than emitting @null@ — Xyne's Zod
-- schema rejects @null@ on the optional @externalId@, @senderName@,
-- @senderEmail@ fields with @VALIDATION_ERROR@ (expected string, received null).
data XyneInboundReq = XyneInboundReq
  { channelId :: Text,
    threadId :: Text,
    subject :: Text,
    body :: Text,
    externalId :: Maybe Text,
    senderName :: Maybe Text,
    senderEmail :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

instance ToJSON XyneInboundReq where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}
  toEncoding = genericToEncoding defaultOptions {omitNothingFields = True}

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
