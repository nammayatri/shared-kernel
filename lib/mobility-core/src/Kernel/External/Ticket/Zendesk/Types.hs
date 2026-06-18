{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Ticket.Zendesk.Types
  ( module Kernel.External.Ticket.Zendesk.Types,
  )
where

import Data.Aeson
import Kernel.Prelude

data ZendeskCreateTicketReq = ZendeskCreateTicketReq
  { ticket :: ZendeskTicketBody
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ZendeskTicketBody = ZendeskTicketBody
  { subject :: Text,
    comment :: ZendeskComment,
    requester :: Maybe ZendeskRequester,
    organizationId :: Maybe Int,
    groupId :: Maybe Int,
    formId :: Maybe Int,
    priority :: Text,
    ticketType :: Text,
    customFields :: [ZendeskCustomField]
  }
  deriving stock (Show, Eq, Generic)

-- Zendesk uses "type" (reserved in Haskell) and snake_case keys
instance ToJSON ZendeskTicketBody where
  toJSON ZendeskTicketBody {..} =
    object $
      [ "subject" .= subject,
        "comment" .= comment,
        "priority" .= priority,
        "type" .= ticketType
      ]
        ++ maybe [] (\r -> ["requester" .= r]) requester
        ++ maybe [] (\oid -> ["organization_id" .= oid]) organizationId
        ++ maybe [] (\gid -> ["group_id" .= gid]) groupId
        ++ maybe [] (\fid -> ["ticket_form_id" .= fid]) formId
        ++ ["custom_fields" .= customFields | not (null customFields)]

instance FromJSON ZendeskTicketBody where
  parseJSON = withObject "ZendeskTicketBody" $ \v ->
    ZendeskTicketBody
      <$> v .: "subject"
      <*> v .: "comment"
      <*> v .:? "requester"
      <*> v .:? "organization_id"
      <*> v .:? "group_id"
      <*> v .:? "ticket_form_id"
      <*> v .: "priority"
      <*> v .: "type"
      <*> (v .:? "custom_fields" .!= [])

data ZendeskComment = ZendeskComment
  { body :: Text,
    htmlBody :: Maybe Text,
    authorId :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ZendeskComment where
  toJSON ZendeskComment {..} =
    object $
      ["body" .= body]
        ++ maybe [] (\h -> ["html_body" .= h]) htmlBody
        ++ maybe [] (\aid -> ["author_id" .= aid]) authorId

instance FromJSON ZendeskComment where
  parseJSON = withObject "ZendeskComment" $ \v ->
    ZendeskComment
      <$> v .: "body"
      <*> v .:? "html_body"
      <*> v .:? "author_id"

data ZendeskRequester = ZendeskRequester
  { name :: Text,
    email :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ZendeskCustomField = ZendeskCustomField
  { fieldId :: Int,
    value :: Value
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ZendeskCustomField where
  toJSON ZendeskCustomField {..} =
    object
      [ "id" .= fieldId,
        "value" .= value
      ]

instance FromJSON ZendeskCustomField where
  parseJSON = withObject "ZendeskCustomField" $ \v ->
    ZendeskCustomField
      <$> v .: "id"
      <*> v .: "value"

data ZendeskCreateTicketResp = ZendeskCreateTicketResp
  { ticket :: ZendeskTicketDetails
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ZendeskTicketDetails = ZendeskTicketDetails
  { id :: Int,
    status :: Maybe Text,
    subject :: Maybe Text,
    requesterId :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ZendeskTicketDetails where
  toJSON ZendeskTicketDetails {..} =
    object $
      ["id" .= id, "status" .= status, "subject" .= subject]
        ++ maybe [] (\r -> ["requester_id" .= r]) requesterId

instance FromJSON ZendeskTicketDetails where
  parseJSON = withObject "ZendeskTicketDetails" $ \v ->
    ZendeskTicketDetails
      <$> v .: "id"
      <*> v .:? "status"
      <*> v .:? "subject"
      <*> v .:? "requester_id"

newtype ZendeskUpdateTicketReq = ZendeskUpdateTicketReq
  { ticket :: ZendeskUpdateTicketBody
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ZendeskUpdateTicketBody = ZendeskUpdateTicketBody
  { comment :: Maybe ZendeskComment,
    status :: Maybe Text,
    groupId :: Maybe Int,
    formId :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ZendeskUpdateTicketBody where
  toJSON ZendeskUpdateTicketBody {..} =
    object $
      maybe [] (\c -> ["comment" .= c]) comment
        ++ maybe [] (\s -> ["status" .= s]) status
        ++ maybe [] (\gid -> ["group_id" .= gid]) groupId
        ++ maybe [] (\fid -> ["ticket_form_id" .= fid]) formId

instance FromJSON ZendeskUpdateTicketBody where
  parseJSON = withObject "ZendeskUpdateTicketBody" $ \v ->
    ZendeskUpdateTicketBody
      <$> v .:? "comment"
      <*> v .:? "status"
      <*> v .:? "group_id"
      <*> v .:? "ticket_form_id"

newtype ZendeskUpdateTicketResp = ZendeskUpdateTicketResp
  { ticket :: ZendeskTicketDetails
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
