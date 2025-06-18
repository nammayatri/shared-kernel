{-# LANGUAGE DeriveAnyClass #-}
{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Kernel.External.SMS.DigoEngage.Types where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson.Types (Options (..))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Text (Text)
import GHC.Generics (Generic)
import Kernel.Prelude (Eq, HasField (hasField), Maybe, Show)
import Servant.Client (BaseUrl)

data SubmitSmsReq = SubmitSmsReq
  { from :: Text,
    to :: Text,
    msg :: Text,
    dlr :: ClientDomain,
    messageType :: Text,
    tiny :: Text,
    tlv :: TLVForSmsReq
  }
  deriving (Generic, Eq, Show)

instance FromJSON SubmitSmsReq where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "messageType" -> "type"
            other -> other
        }

instance ToJSON SubmitSmsReq where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "messageType" -> "type"
            other -> other
        }

data ClientDomain = ClientDomain
  { mask :: Int,
    url :: BaseUrl
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

customOptions :: Options
customOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "peId" -> "PE_ID"
        "templateId" -> "TEMPLATE_ID"
        "telemarketerId" -> "TELEMARKETER_ID"
        other -> other
    }

data TLVForSmsReq = TLVForSmsReq
  { peId :: Text,
    templateId :: Text,
    telemarketerId :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON TLVForSmsReq where
  parseJSON = genericParseJSON customOptions

instance ToJSON TLVForSmsReq where
  toJSON = genericToJSON customOptions

data TLVForSmsCfg = TLVForSmsCfg
  { peId :: Text,
    telemarketerId :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON TLVForSmsCfg where
  parseJSON = genericParseJSON customOptions

instance ToJSON TLVForSmsCfg where
  toJSON = genericToJSON customOptions

-- Response when the SMS is successfully sent
data SmsSuccessResponse = SmsSuccessResponse
  { id :: Text
  }
  deriving (Show, Generic)

instance FromJSON SmsSuccessResponse

instance ToJSON SmsSuccessResponse

-- Response when there's an error (e.g., validation error)
data SmsErrorResponse = SmsErrorResponse
  { status :: Int,
    message :: Text,
    errorDetails :: Maybe [SmsErrorDetail] -- Some errors might not have "data"
  }
  deriving (Show, Generic)

instance FromJSON SmsErrorResponse

instance ToJSON SmsErrorResponse

-- Structure of validation error details
data SmsErrorDetail = SmsErrorDetail
  { msg :: Text,
    param :: Text,
    location :: Text
  }
  deriving (Show, Generic)

instance FromJSON SmsErrorDetail

instance ToJSON SmsErrorDetail

-- Final type to wrap both cases
data SubmitSmsRes
  = SmsSuccess SmsSuccessResponse
  | SmsError SmsErrorResponse
  deriving (Show, Generic)

instance FromJSON SubmitSmsRes where
  parseJSON v =
    (SmsSuccess <$> parseJSON v) <|> (SmsError <$> parseJSON v)

instance ToJSON SubmitSmsRes where
  toJSON (SmsSuccess res) = toJSON res
  toJSON (SmsError err) = toJSON err
