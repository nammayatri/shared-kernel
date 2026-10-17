{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.EventTracking.Breeze.Types where

import Data.Aeson (Value)
import Kernel.Prelude
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)

-- | One BreezeIQ event envelope. The wire names are fixed by Breeze, including
-- the deliberate spellings @envelop_version@ and @occured_at@: events with the
-- "correct" spellings are rejected.
data BreezeEvent = BreezeEvent
  { envelop_version :: Text,
    -- | Underscore-prefixed to avoid clashing with 'Prelude.id';
    -- 'stripPrefixUnderscoreIfAny' restores the wire name @id@.
    _id :: Text,
    name :: Text,
    tenant_id :: Text,
    workspace_id :: Text,
    anon_id :: Text,
    -- | RFC 3339, UTC, whole seconds (the format Breeze documents).
    occured_at :: Text,
    properties :: Value
  }
  deriving (Show, Generic)

instance ToJSON BreezeEvent where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON BreezeEvent where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

-- | The collector answers 200 as soon as events are queued, so this only says
-- what was accepted for processing. @filtered@ counts events whose name the
-- collector's allow-list rejects. An event that fails validation later (for
-- example an unregistered name, or no usable identifier) is set aside after the
-- 200 and is not reported here.
data BreezeEventResp = BreezeEventResp
  { collected :: Maybe Int,
    filtered :: Maybe Int,
    total :: Maybe Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)
