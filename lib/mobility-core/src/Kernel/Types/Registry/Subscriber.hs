module Kernel.Types.Registry.Subscriber where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding ((.=))
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
