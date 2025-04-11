{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Whatsapp.TataCommunications.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import GHC.Generics hiding (from, to)
import Kernel.Prelude hiding (error)

-- Top-level request type
data WhatsAppMessageReq = WhatsAppMessageReq
  { recipient :: Text,
    from :: Text,
    type_ :: Text,
    template :: Template
  }
  deriving (Show, Generic)

instance ToJSON WhatsAppMessageReq where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = renameFields}

instance FromJSON WhatsAppMessageReq where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = renameFields}

-- Template structure
data Template = Template
  { name :: Text,
    language :: Language,
    components :: [Component]
  }
  deriving (Show, Generic)

instance ToJSON Template

instance FromJSON Template

-- Language structure
data Language = Language
  { code :: Text
  }
  deriving (Show, Generic)

instance ToJSON Language

instance FromJSON Language

-- Components structure
data Component = Component
  { type_ :: Text,
    parameters :: [Parameter]
  }
  deriving (Show, Generic)

instance ToJSON Component where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = renameFields}

instance FromJSON Component where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = renameFields}

-- Parameters structure
data Parameter = Parameter
  { type_ :: Text,
    text :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON Parameter where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = renameFields}

instance FromJSON Parameter where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = renameFields}

data WhatsappSubmitRes
  = WhatsappSuccess WhatsAppResponse
  | WhatsAppError WhatsAppErrorResponse
  deriving (Show, Generic)

instance FromJSON WhatsappSubmitRes where
  parseJSON v =
    (WhatsappSuccess <$> parseJSON v) <|> (WhatsAppError <$> parseJSON v)

instance ToJSON WhatsappSubmitRes where
  toJSON (WhatsappSuccess res) = toJSON res
  toJSON (WhatsAppError err) = toJSON err

data WhatsAppResponse = WhatsAppResponse
  { id :: Text
  }
  deriving (Show, Generic)

instance ToJSON WhatsAppResponse

instance FromJSON WhatsAppResponse

data WhatsAppErrorResponse = WhatsAppErrorResponse
  { errDetails :: WhatsAppErrorDetails
  }
  deriving (Show, Generic)

instance ToJSON WhatsAppErrorResponse where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = renameFields}

instance FromJSON WhatsAppErrorResponse where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = renameFields}

data WhatsAppErrorDetails = WhatsAppErrorDetails
  { code :: Int,
    message :: Text,
    type_ :: Text
  }
  deriving (Show, Generic)

instance ToJSON WhatsAppErrorDetails where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = renameFields}

instance FromJSON WhatsAppErrorDetails where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = renameFields}

-- Helper function to rename fields
renameFields :: String -> String
renameFields "recipient" = "to"
renameFields "errDetails" = "error"
renameFields "type_" = "type"
renameFields other = other
