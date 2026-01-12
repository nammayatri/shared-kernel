{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Whatsapp.Karix.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Map.Strict (Map)
import GHC.Generics hiding (from, to)
import Kernel.Prelude hiding (error)

-- Top-level request type
data KarixWhatsAppMessageReq = KarixWhatsAppMessageReq
  { message :: KarixMessage,
    metaData :: Maybe KarixMetaData
  }
  deriving (Show, Generic)

instance ToJSON KarixWhatsAppMessageReq where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON KarixWhatsAppMessageReq where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

-- Message structure
data KarixMessage = KarixMessage
  { channel :: Text,
    content :: KarixContent,
    recipient :: KarixRecipient,
    sender :: KarixSender,
    preferences :: Maybe KarixPreferences
  }
  deriving (Show, Generic)

instance ToJSON KarixMessage where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON KarixMessage where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

-- Content structure - supports both TEMPLATE and MEDIA_TEMPLATE
data KarixContent = KarixContent
  { preview_url :: Maybe Bool,
    type_ :: Text,
    mediaTemplate :: Maybe KarixTemplate,
    shorten_url :: Maybe Bool
  }
  deriving (Show, Generic)

instance ToJSON KarixContent where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = renameContentFields}

instance FromJSON KarixContent where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = renameContentFields}

renameContentFields :: String -> String
renameContentFields "type_" = "type"
renameContentFields "preview_url" = "preview_url"
renameContentFields "shorten_url" = "shorten_url"
renameContentFields other = other

-- Template structure for simple template messages
data KarixTemplate = KarixTemplate
  { autoTemplate :: Text,
    buttons :: Maybe (Map Text Text)
  }
  deriving (Show, Generic)

instance ToJSON KarixTemplate where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON KarixTemplate where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

-- Media structure
data KarixMedia = KarixMedia
  { type_ :: Text,
    url :: Text,
    fileName :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON KarixMedia where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = renameMediaFields}

instance FromJSON KarixMedia where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = renameMediaFields}

renameMediaFields :: String -> String
renameMediaFields "type_" = "type"
renameMediaFields "fileName" = "fileName"
renameMediaFields other = other

-- Recipient structure
data KarixRecipient = KarixRecipient
  { to :: Text,
    recipient_type :: Text,
    reference :: Maybe KarixReference
  }
  deriving (Show, Generic)

instance ToJSON KarixRecipient where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON KarixRecipient where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

-- Reference structure
data KarixReference = KarixReference
  { cust_ref :: Maybe Text,
    messageTag1 :: Maybe Text,
    conversationId :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON KarixReference where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON KarixReference where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

-- Sender structure
data KarixSender = KarixSender
  { from :: Text
  }
  deriving (Show, Generic)

instance ToJSON KarixSender where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON KarixSender where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

-- Preferences structure
data KarixPreferences = KarixPreferences
  { webHookDNId :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON KarixPreferences where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON KarixPreferences where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

-- MetaData structure
data KarixMetaData = KarixMetaData
  { version :: Text
  }
  deriving (Show, Generic)

instance ToJSON KarixMetaData where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON KarixMetaData where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

-- Response types
data KarixWhatsappSubmitRes
  = KarixWhatsappSuccess KarixWhatsAppResponse
  | KarixWhatsAppError KarixWhatsAppErrorResponse
  deriving (Show, Generic)

instance FromJSON KarixWhatsappSubmitRes where
  parseJSON v =
    (KarixWhatsappSuccess <$> parseJSON v) <|> (KarixWhatsAppError <$> parseJSON v)

instance ToJSON KarixWhatsappSubmitRes where
  toJSON (KarixWhatsappSuccess res) = toJSON res
  toJSON (KarixWhatsAppError err) = toJSON err

data KarixWhatsAppResponse = KarixWhatsAppResponse
  { id :: Maybe Text,
    status :: Maybe Text,
    message :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON KarixWhatsAppResponse

instance FromJSON KarixWhatsAppResponse

data KarixWhatsAppErrorResponse = KarixWhatsAppErrorResponse
  { error :: KarixWhatsAppErrorDetails
  }
  deriving (Show, Generic)

instance ToJSON KarixWhatsAppErrorResponse

instance FromJSON KarixWhatsAppErrorResponse

data KarixWhatsAppErrorDetails = KarixWhatsAppErrorDetails
  { code :: Maybe Int,
    message :: Text,
    type_ :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON KarixWhatsAppErrorDetails where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = renameErrorFields}

instance FromJSON KarixWhatsAppErrorDetails where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = renameErrorFields}

renameErrorFields :: String -> String
renameErrorFields "type_" = "type"
renameErrorFields other = other
