{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.EventTracking.Clevertap.Types where

import Data.Aeson (Value)
import Kernel.Prelude
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)

-- | Clevertap's upload endpoint takes a batch under the key @d@. We currently
-- send a single event per call; the list shape is the vendor's, not ours.
newtype ClevertapUploadReq = ClevertapUploadReq
  { d :: [ClevertapEvent]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ClevertapEvent = ClevertapEvent
  { -- | Underscore-prefixed to avoid clashing with 'Kernel.Prelude.identity';
    -- 'stripPrefixUnderscoreIfAny' restores the wire name @identity@.
    _identity :: Text,
    _type :: Text,
    evtName :: Text,
    evtData :: Value,
    -- | Event time as a Unix epoch, per Clevertap's @ts@ field.
    ts :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON ClevertapEvent where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON ClevertapEvent where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

-- | Clevertap reports per-event failures in @unprocessed@ while still
-- returning HTTP 200, so this response must be inspected rather than ignored.
--
-- @status@ is one of @success@, @partial@ or @fail@; we key off @unprocessed@
-- rather than @status@ so a partial success is not mistaken for a clean one.
data ClevertapUploadResp = ClevertapUploadResp
  { status :: Text,
    processed :: Maybe Int,
    unprocessed :: Maybe [ClevertapUnprocessed]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | A single rejected event. @code@ carries Clevertap's error number, e.g. 527
-- for a timestamp outside the allowed ingestion window.
data ClevertapUnprocessed = ClevertapUnprocessed
  { status :: Text,
    code :: Maybe Int,
    -- | Underscore-prefixed to keep clear of 'Prelude.error';
    -- 'stripPrefixUnderscoreIfAny' restores the wire name @error@.
    _error :: Maybe Text,
    record :: Maybe Value
  }
  deriving (Show, Generic)

instance ToJSON ClevertapUnprocessed where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON ClevertapUnprocessed where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny
