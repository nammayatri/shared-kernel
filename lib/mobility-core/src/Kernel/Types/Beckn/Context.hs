{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Kernel.Types.Beckn.Context (module Kernel.Types.Beckn.Context, module Reexport) where

import Data.Aeson
import Data.Maybe (fromJust)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Kernel.Types.App
import Kernel.Types.Beckn.City as Reexport
import Kernel.Types.Beckn.Country as Reexport
import Kernel.Types.Beckn.Domain as Reexport
import Kernel.Types.Beckn.IndianState as Reexport
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))
import Kernel.Utils.Example
import Kernel.Utils.GenericPretty
import Kernel.Utils.JSON
import Servant.Client (parseBaseUrl)

data Context = Context
  { domain :: Domain,
    country :: Country,
    city :: City,
    action :: Action,
    core_version :: Text,
    bap_id :: Text,
    bap_uri :: BaseUrl,
    bpp_id :: Maybe Text,
    bpp_uri :: Maybe BaseUrl,
    transaction_id :: Maybe Text,
    message_id :: Text,
    timestamp :: UTCTimeRFC3339,
    max_callbacks :: Maybe Int
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
        country = India,
        city = Kochi,
        max_callbacks = Just 1
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
  | ISSUE
  | ON_ISSUE
  | ISSUE_STATUS
  | ON_ISSUE_STATUS
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
  ISSUE -> Just ON_ISSUE
  ISSUE_STATUS -> Just ON_ISSUE_STATUS
  _ -> Nothing

getSubscriberType :: Action -> Subscriber.SubscriberType
getSubscriberType action = if isNothing (mapToCbAction action) then Subscriber.BPP else Subscriber.BAP
