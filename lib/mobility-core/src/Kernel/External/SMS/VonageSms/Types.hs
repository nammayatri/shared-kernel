{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveGeneric #-}

module Kernel.External.SMS.VonageSms.Types where

import Control.Applicative ((<$>), (<|>))
import Data.Aeson (FromJSON (..), Options (..), ToJSON (..), defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import GHC.Generics (Generic)
import Kernel.Prelude (Eq, Maybe, Show, ($))
import Web.FormUrlEncoded (Form (..), ToForm, toForm)

-- Simple request structure for Vonage SMS API
data VonageSmsReq = VonageSmsReq
  { apiKey :: Text,
    apiSecret :: Text,
    from :: Text,
    to :: Text,
    text :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON VonageSmsReq where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "apiKey" -> "api_key"
            "apiSecret" -> "api_secret"
            other -> other
        }

instance ToJSON VonageSmsReq where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "apiKey" -> "api_key"
            "apiSecret" -> "api_secret"
            other -> other
        }

instance ToForm VonageSmsReq where
  toForm VonageSmsReq {..} =
    Form $
      HM.fromList
        [ ("api_key", [apiKey]),
          ("api_secret", [apiSecret]),
          ("from", [from]),
          ("to", [to]),
          ("text", [text])
        ]

-- Simple response structure for Vonage SMS API
data VonageSmsRes = VonageSmsRes
  { messageCount :: Text,
    messages :: [VonageMessage]
  }
  deriving (Show, Eq, Generic)

instance FromJSON VonageSmsRes where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "messageCount" -> "message-count"
            other -> other
        }

instance ToJSON VonageSmsRes where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "messageCount" -> "message-count"
            other -> other
        }

-- Message response matching actual Vonage API response
data VonageMessage = VonageMessage
  { to :: Text,
    messageId :: Text,
    status :: Text,
    remainingBalance :: Maybe Text,
    messagePrice :: Maybe Text,
    network :: Maybe Text,
    errorText :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON VonageMessage where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "messageId" -> "message-id"
            "remainingBalance" -> "remaining-balance"
            "messagePrice" -> "message-price"
            "errorText" -> "error-text"
            other -> other
        }

instance ToJSON VonageMessage where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "messageId" -> "message-id"
            "remainingBalance" -> "remaining-balance"
            "messagePrice" -> "message-price"
            "errorText" -> "error-text"
            other -> other
        }

-- Error response structure for Vonage SMS API
data VonageErrorResponse = VonageErrorResponse
  { messageCount :: Text,
    messages :: [VonageErrorMessage]
  }
  deriving (Show, Eq, Generic)

instance FromJSON VonageErrorResponse where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "messageCount" -> "message-count"
            other -> other
        }

instance ToJSON VonageErrorResponse where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "messageCount" -> "message-count"
            other -> other
        }

-- Error message structure
data VonageErrorMessage = VonageErrorMessage
  { status :: Text,
    errorText :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON VonageErrorMessage where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "errorText" -> "error-text"
            other -> other
        }

instance ToJSON VonageErrorMessage where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "errorText" -> "error-text"
            other -> other
        }

-- Unified response type for consistency with other SMS providers
data SubmitSmsRes = SmsSuccess VonageSmsRes | SmsError VonageErrorResponse
  deriving (Show, Eq, Generic)

instance FromJSON SubmitSmsRes where
  parseJSON v = (SmsSuccess <$> parseJSON v) <|> (SmsError <$> parseJSON v)

instance ToJSON SubmitSmsRes where
  toJSON (SmsSuccess res) = toJSON res
  toJSON (SmsError err) = toJSON err
