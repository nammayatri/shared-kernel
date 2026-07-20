{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Meta.Types where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.JSON (camelToSnakeCase)

--------------------------------------------------------------------------------
-- Shared aeson options: ONE mechanism for every generic instance below.
-- omitNothingFields works BOTH directions (encode drops Nothing; generic parse
-- accepts absent keys for Maybe fields).
--------------------------------------------------------------------------------

metaFieldModifier :: String -> String
metaFieldModifier "type_" = "type" -- reserved-word remap
metaFieldModifier "object_" = "object" -- avoid clash with Data.Aeson.object
metaFieldModifier s = camelToSnakeCase s

metaAesonOptions :: Options
metaAesonOptions =
  defaultOptions
    { fieldLabelModifier = metaFieldModifier,
      omitNothingFields = True
    }

--------------------------------------------------------------------------------
-- OUTBOUND: one envelope record + Maybe payload fields (KarixContent idiom).
-- Wire constants (messaging_product, recipient_type, type) are set by the
-- smart constructors in Kernel.External.Meta; illegal payload combos are
-- prevented there, not by the type.
--------------------------------------------------------------------------------

data MetaSendMessageReq = MetaSendMessageReq
  { messagingProduct :: Text, -- "messaging_product" = "whatsapp"
    recipientType :: Maybe Text, -- "recipient_type" = Just "individual"
    to :: Text,
    type_ :: Text, -- "text" | "interactive" | "video"
    text :: Maybe MetaTextBody,
    interactive :: Maybe MetaInteractive,
    video :: Maybe MetaVideo,
    context :: Maybe MetaMessageContext,
    bizOpaqueCallbackData :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaSendMessageReq where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaSendMessageReq where
  parseJSON = genericParseJSON metaAesonOptions

data MetaTextBody = MetaTextBody
  { body :: Text,
    previewUrl :: Maybe Bool -- "preview_url"
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaTextBody where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaTextBody where
  parseJSON = genericParseJSON metaAesonOptions

newtype MetaMessageContext = MetaMessageContext
  { messageId :: Text -- "message_id"
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaMessageContext where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaMessageContext where
  parseJSON = genericParseJSON metaAesonOptions

data MetaInteractive = MetaInteractive
  { type_ :: Text, -- "button" | "list" | "location_request_message"
    header :: Maybe MetaHeader,
    body :: MetaTextObject,
    footer :: Maybe MetaTextObject,
    action :: MetaInteractiveAction
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaInteractive where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaInteractive where
  parseJSON = genericParseJSON metaAesonOptions

newtype MetaTextObject = MetaTextObject
  { text :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaTextObject where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaTextObject where
  parseJSON = genericParseJSON metaAesonOptions

data MetaHeader = MetaHeader
  { type_ :: Text,
    text :: Maybe Text,
    image :: Maybe MetaMediaRef,
    video :: Maybe MetaMediaRef,
    document :: Maybe MetaMediaRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaHeader where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaHeader where
  parseJSON = genericParseJSON metaAesonOptions

data MetaMediaRef = MetaMediaRef
  { id :: Maybe Text,
    link :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaMediaRef where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaMediaRef where
  parseJSON = genericParseJSON metaAesonOptions

data MetaInteractiveAction = MetaInteractiveAction
  { buttons :: Maybe [MetaReplyButton], -- reply-buttons shape
    button :: Maybe Text, -- list shape (label)
    sections :: Maybe [MetaListSection], -- list shape
    name :: Maybe Text -- location_request shape = Just "send_location"
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaInteractiveAction where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaInteractiveAction where
  parseJSON = genericParseJSON metaAesonOptions

data MetaReplyButton = MetaReplyButton
  { type_ :: Text, -- "reply"
    reply :: MetaButtonRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaReplyButton where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaReplyButton where
  parseJSON = genericParseJSON metaAesonOptions

data MetaButtonRef = MetaButtonRef
  { id :: Text,
    title :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaButtonRef where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaButtonRef where
  parseJSON = genericParseJSON metaAesonOptions

data MetaListSection = MetaListSection
  { title :: Maybe Text,
    rows :: [MetaListRow]
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaListSection where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaListSection where
  parseJSON = genericParseJSON metaAesonOptions

data MetaListRow = MetaListRow
  { id :: Text,
    title :: Text,
    description :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaListRow where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaListRow where
  parseJSON = genericParseJSON metaAesonOptions

data MetaVideo = MetaVideo
  { link :: Maybe Text,
    id :: Maybe Text,
    caption :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaVideo where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaVideo where
  parseJSON = genericParseJSON metaAesonOptions

-- NOTE: the send-response is NOT one of the golden-verified units. Kept lenient
-- (all-Maybe) and intentionally uncovered by a golden test. `id` on
-- MetaRespMessage is the wamid — the only outbound correlation handle.
data MetaSendMessageResp = MetaSendMessageResp
  { messagingProduct :: Maybe Text,
    contacts :: Maybe [MetaRespContact],
    messages :: Maybe [MetaRespMessage]
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaSendMessageResp where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaSendMessageResp where
  parseJSON = genericParseJSON metaAesonOptions

data MetaRespContact = MetaRespContact
  { input :: Maybe Text,
    waId :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaRespContact where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaRespContact where
  parseJSON = genericParseJSON metaAesonOptions

data MetaRespMessage = MetaRespMessage
  { id :: Text,
    messageStatus :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaRespMessage where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaRespMessage where
  parseJSON = genericParseJSON metaAesonOptions

--------------------------------------------------------------------------------
-- INBOUND: webhook envelope chain (generic except the two hand-written spots).
--------------------------------------------------------------------------------

data MetaWebhookEnvelope = MetaWebhookEnvelope
  { object_ :: Text,
    entry :: [MetaWebhookEntry]
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaWebhookEnvelope where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaWebhookEnvelope where
  parseJSON = genericParseJSON metaAesonOptions

-- NB: WhatsApp WABA entries carry no "time" field — do not add one.
data MetaWebhookEntry = MetaWebhookEntry
  { id :: Text,
    changes :: [MetaWebhookChange]
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaWebhookEntry where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaWebhookEntry where
  parseJSON = genericParseJSON metaAesonOptions

data MetaWebhookChange = MetaWebhookChange
  { value :: MetaChangeValue,
    field :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaWebhookChange where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaWebhookChange where
  parseJSON = genericParseJSON metaAesonOptions

data MetaChangeValue = MetaChangeValue
  { messagingProduct :: Text,
    metadata :: MetaWebhookMetadata,
    contacts :: Maybe [MetaContact],
    messages :: Maybe [MetaInboundMessage],
    statuses :: Maybe [MetaStatus],
    errors :: Maybe [MetaWaError]
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaChangeValue where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaChangeValue where
  parseJSON = genericParseJSON metaAesonOptions

data MetaWebhookMetadata = MetaWebhookMetadata
  { displayPhoneNumber :: Text,
    phoneNumberId :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaWebhookMetadata where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaWebhookMetadata where
  parseJSON = genericParseJSON metaAesonOptions

data MetaContact = MetaContact
  { profile :: Maybe MetaProfile,
    waId :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaContact where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaContact where
  parseJSON = genericParseJSON metaAesonOptions

newtype MetaProfile = MetaProfile
  { name :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaProfile where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaProfile where
  parseJSON = genericParseJSON metaAesonOptions

-- Unknown message type_ (reaction/image/...) still decodes; payloads all
-- Nothing. NEVER fail on an unknown type. timestamp is a STRING epoch, keep it
-- Text (never Int).
data MetaInboundMessage = MetaInboundMessage
  { from :: Text,
    id :: Text,
    timestamp :: Text,
    type_ :: Text, -- raw discriminator retained
    text :: Maybe MetaInboundText,
    interactive :: Maybe MetaInteractiveReply,
    location :: Maybe MetaInboundLocation,
    context :: Maybe MetaInboundContext,
    referral :: Maybe Value,
    errors :: Maybe [Value]
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaInboundMessage where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaInboundMessage where
  parseJSON = genericParseJSON metaAesonOptions

newtype MetaInboundText = MetaInboundText
  { body :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaInboundText where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaInboundText where
  parseJSON = genericParseJSON metaAesonOptions

data MetaInboundContext = MetaInboundContext
  { from :: Maybe Text,
    id :: Maybe Text,
    forwarded :: Maybe Bool,
    frequentlyForwarded :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaInboundContext where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaInboundContext where
  parseJSON = genericParseJSON metaAesonOptions

-- latitude/longitude are JSON NUMBERS (Double), never strings.
data MetaInboundLocation = MetaInboundLocation
  { latitude :: Double,
    longitude :: Double,
    name :: Maybe Text,
    address :: Maybe Text,
    url :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaInboundLocation where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaInboundLocation where
  parseJSON = genericParseJSON metaAesonOptions

--------------------------------------------------------------------------------
-- HAND-WRITTEN #1: interactive reply sum on interactive.type.
-- The type-safety improvement over the TS ||-fallthrough. Unknown types decode
-- to MetaUnknownReply (decode SUCCEEDS — batched envelopes must survive); the
-- engine ignores them. Payload records are wrapped (no naked record fields in
-- constructors) to avoid partial selectors under -Werror.
--------------------------------------------------------------------------------

data MetaInteractiveReply
  = MetaButtonReply MetaButtonReplyData
  | MetaListReply MetaListReplyData
  | MetaUnknownReply Text
  deriving (Show, Eq, Generic)

data MetaButtonReplyData = MetaButtonReplyData
  { id :: Text,
    title :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaButtonReplyData where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaButtonReplyData where
  parseJSON = genericParseJSON metaAesonOptions

data MetaListReplyData = MetaListReplyData
  { id :: Text,
    title :: Text,
    description :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaListReplyData where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaListReplyData where
  parseJSON = genericParseJSON metaAesonOptions

instance FromJSON MetaInteractiveReply where
  parseJSON = withObject "MetaInteractiveReply" $ \o -> do
    t <- o .: "type"
    case (t :: Text) of
      "button_reply" -> MetaButtonReply <$> o .: "button_reply"
      "list_reply" -> MetaListReply <$> o .: "list_reply"
      other -> pure (MetaUnknownReply other)

instance ToJSON MetaInteractiveReply where
  toJSON (MetaButtonReply d) = object ["type" .= ("button_reply" :: Text), "button_reply" .= d]
  toJSON (MetaListReply d) = object ["type" .= ("list_reply" :: Text), "list_reply" .= d]
  toJSON (MetaUnknownReply t) = object ["type" .= t]

--------------------------------------------------------------------------------
-- Statuses.
--------------------------------------------------------------------------------

data MetaStatus = MetaStatus
  { id :: Text,
    status :: MetaStatusValue,
    timestamp :: Text,
    recipientId :: Text,
    recipientType :: Maybe Text,
    bizOpaqueCallbackData :: Maybe Text,
    conversation :: Maybe MetaConversation,
    pricing :: Maybe MetaPricing,
    errors :: Maybe [MetaWaError]
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaStatus where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaStatus where
  parseJSON = genericParseJSON metaAesonOptions

data MetaConversation = MetaConversation
  { id :: Text,
    expirationTimestamp :: Maybe Text,
    origin :: Maybe MetaOrigin
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaConversation where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaConversation where
  parseJSON = genericParseJSON metaAesonOptions

newtype MetaOrigin = MetaOrigin
  { type_ :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaOrigin where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaOrigin where
  parseJSON = genericParseJSON metaAesonOptions

data MetaPricing = MetaPricing
  { billable :: Maybe Bool,
    pricingModel :: Maybe Text,
    type_ :: Maybe Text,
    category :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaPricing where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaPricing where
  parseJSON = genericParseJSON metaAesonOptions

data MetaWaError = MetaWaError
  { code :: Int,
    title :: Maybe Text,
    message :: Maybe Text,
    errorData :: Maybe MetaErrorData,
    href :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaWaError where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaWaError where
  parseJSON = genericParseJSON metaAesonOptions

newtype MetaErrorData = MetaErrorData
  { details :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MetaErrorData where
  toJSON = genericToJSON metaAesonOptions

instance FromJSON MetaErrorData where
  parseJSON = genericParseJSON metaAesonOptions

--------------------------------------------------------------------------------
-- HAND-WRITTEN #2: status enum with unknown-tolerance (forward compat — MUST
-- NOT fail on an unknown status).
--------------------------------------------------------------------------------

data MetaStatusValue
  = MetaSent
  | MetaDelivered
  | MetaRead
  | MetaFailed
  | MetaPlayed
  | MetaUnknownStatus Text
  deriving (Show, Eq, Generic)

instance FromJSON MetaStatusValue where
  parseJSON = withText "MetaStatusValue" $ \t ->
    pure $ case t of
      "sent" -> MetaSent
      "delivered" -> MetaDelivered
      "read" -> MetaRead
      "failed" -> MetaFailed
      "played" -> MetaPlayed
      other -> MetaUnknownStatus other

instance ToJSON MetaStatusValue where
  toJSON MetaSent = String "sent"
  toJSON MetaDelivered = String "delivered"
  toJSON MetaRead = String "read"
  toJSON MetaFailed = String "failed"
  toJSON MetaPlayed = String "played"
  toJSON (MetaUnknownStatus t) = String t

--------------------------------------------------------------------------------
-- Dispatch: route on WHICH array is present, never on the `field` string.
--------------------------------------------------------------------------------

data MetaWebhookEvent
  = EventMessages MetaWebhookMetadata [MetaContact] [MetaInboundMessage]
  | EventStatuses [MetaStatus]
  | EventErrors [MetaWaError]
  | EventUnknown
  deriving (Show, Eq, Generic)

classifyChangeValue :: MetaChangeValue -> MetaWebhookEvent
classifyChangeValue cv =
  case (cv.messages, cv.statuses, cv.errors) of
    (Just msgs, _, _) | not (null msgs) -> EventMessages cv.metadata (fromMaybe [] cv.contacts) msgs
    (_, Just sts, _) | not (null sts) -> EventStatuses sts
    (_, _, Just errs) | not (null errs) -> EventErrors errs
    _ -> EventUnknown
