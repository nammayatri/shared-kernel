{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Whatsapp.Cerf.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Text as T
import Kernel.Prelude

-- No Show instance: the request carries the decrypted apiKey.
data CerfWhatsappReq = CerfWhatsappReq
  { apiKey :: Text,
    campaignName :: Text,
    destination :: Text,
    userName :: Text,
    templateParams :: [Text],
    buttons :: Maybe [CerfButton]
  }
  deriving (Generic)

instance ToJSON CerfWhatsappReq where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

-- | Button params, used for the copy-code button of authentication (OTP) templates.
data CerfButton = CerfButton
  { _type :: Text,
    sub_type :: Text,
    index :: Int,
    parameters :: [CerfButtonParameter]
  }
  deriving (Generic)

instance ToJSON CerfButton where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = dropUnderscorePrefix}

data CerfButtonParameter = CerfButtonParameter
  { _type :: Text,
    text :: Text
  }
  deriving (Generic)

instance ToJSON CerfButtonParameter where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = dropUnderscorePrefix}

dropUnderscorePrefix :: String -> String
dropUnderscorePrefix ('_' : rest) = rest
dropUnderscorePrefix other = other

mkCopyCodeButton :: Text -> CerfButton
mkCopyCodeButton code =
  CerfButton
    { _type = "button",
      sub_type = "url",
      index = 0,
      parameters = [CerfButtonParameter {_type = "text", text = code}]
    }

data CerfWhatsappRes = CerfWhatsappRes
  { success :: Bool,
    submittedMessageId :: Maybe Text,
    message :: Maybe Text
  }
  deriving (Show, Generic, ToJSON)

instance FromJSON CerfWhatsappRes where
  parseJSON = withObject "CerfWhatsappRes" $ \o -> do
    successVal <- o .:? "success"
    messageId <- o .:? "submitted_message_id"
    mbMessage <- o .:? "message"
    mbErrorMessage <- o .:? "errorMessage"
    pure
      CerfWhatsappRes
        { success = parseSuccess successVal,
          submittedMessageId = messageId,
          message = mbMessage <|> mbErrorMessage
        }

-- CERF returns "success" as a string ("true"/"false"); accept a bool as well.
parseSuccess :: Maybe Value -> Bool
parseSuccess = \case
  Just (Bool b) -> b
  Just (String s) -> T.toLower s == "true"
  _ -> False
