{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Registry.Subscriber where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude
import Kernel.Types.Base64
import Kernel.Types.Registry.Domain (Domain)
import Kernel.Utils.Dhall (FromDhall)
import Servant.Client (BaseUrl)

data Subscriber = Subscriber
  { unique_key_id :: Text,
    subscriber_id :: Text,
    subscriber_url :: BaseUrl,
    _type :: SubscriberType,
    domain :: Domain,
    city :: Maybe Text,
    country :: Maybe Text,
    signing_public_key :: Base64,
    encr_public_key :: Maybe Base64,
    valid_from :: Maybe UTCTime,
    valid_until :: Maybe UTCTime,
    status :: Maybe SubscriberStatus,
    created :: UTCTime,
    updated :: UTCTime
  }
  deriving (Show, Generic)

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "unique_key_id" -> "ukId"
        "_type" -> "type"
        other -> other
    }

instance FromJSON Subscriber where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Subscriber where
  toJSON = genericToJSON jsonOptions

data SubscriberType
  = BAP
  | BPP
  | BG
  | LREG
  | CREG
  | RREG
  deriving (Show, Read, Generic, Eq, ToSchema, FromJSON, ToJSON, FromDhall)

data SubscriberStatus
  = INITIATED
  | UNDER_SUBSCRIPTION
  | SUBSCRIBED
  | EXPIRED
  | UNSUBSCRIBED
  | INVALID_SSL
  deriving (Show, Read, Generic, FromJSON, ToJSON)
