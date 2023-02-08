{-# LANGUAGE DerivingVia #-}

module Kernel.Types.Beckn.Context (module Kernel.Types.Beckn.Context, module Reexport) where

import Kernel.Types.App
import Kernel.Types.Beckn.Domain as Reexport
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))
import Kernel.Utils.Example
import Kernel.Utils.GenericPretty
import Kernel.Utils.JSON
import Data.Aeson
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Servant.Client (parseBaseUrl)

data Context = Context
  { domain :: Domain,
    country :: Text,
    city :: Text,
    action :: Action,
    core_version :: Text,
    bap_id :: Text,
    bap_uri :: BaseUrl,
    bpp_id :: Maybe Text,
    bpp_uri :: Maybe BaseUrl,
    transaction_id :: Maybe Text,
    message_id :: Text,
    timestamp :: UTCTimeRFC3339
  }
  deriving (Generic, FromJSON, Show, ToSchema, PrettyShow)

instance ToJSON Context where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}

instance Example Context where
  example =
    Context
      { domain = example,
        action = example,
        core_version = "0.9.3",
        bap_id = "API.DOMAIN",
        bap_uri = fromJust $ parseBaseUrl "https://api.domain.com/",
        bpp_id = Just "API.DOMAIN",
        bpp_uri = parseBaseUrl "https://api.domain.com/",
        transaction_id = Just idExample,
        message_id = idExample,
        timestamp = UTCTimeRFC3339 example,
        country = "IND",
        city = "Kochi"
      }

data Action
  = SEARCH
  | SELECT
  | INIT
  | CONFIRM
  | UPDATE
  | STATUS
  | TRACK
  | CANCEL
  | RATING
  | SUPPORT
  | ON_SEARCH
  | ON_SELECT
  | ON_INIT
  | ON_CONFIRM
  | ON_UPDATE
  | ON_STATUS
  | ON_TRACK
  | ON_CANCEL
  | ON_RATING
  | ON_SUPPORT
  deriving (Generic, Show, Eq, ToSchema)
  deriving (PrettyShow) via Showable Action

instance FromJSON Action where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON Action where
  toJSON = genericToJSON constructorsToLowerOptions

instance Example Action where
  example = SEARCH

mapToCbAction :: Action -> Maybe Action
mapToCbAction = \case
  SEARCH -> Just ON_SEARCH
  SELECT -> Just ON_SELECT
  INIT -> Just ON_INIT
  CONFIRM -> Just ON_CONFIRM
  UPDATE -> Just ON_UPDATE
  STATUS -> Just ON_STATUS
  TRACK -> Just ON_TRACK
  CANCEL -> Just ON_CANCEL
  RATING -> Just ON_RATING
  SUPPORT -> Just ON_SUPPORT
  _ -> Nothing
