{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.EventTracking.FirebaseAnalytics.Types where

import qualified Data.Aeson as A
import Kernel.Prelude

-- | Body of @POST /mp/collect@; field names are the wire names.
data MpCollectReq = MpCollectReq
  { -- | From the Analytics SDK's @getAppInstanceId@, not the Firebase installation id.
    app_instance_id :: Text,
    user_id :: Maybe Text,
    timestamp_micros :: Maybe Integer,
    -- | Only set for debug requests. Google defaults to @RELAXED@, which ignores
    -- values that exceed limits, so validation must be asked for explicitly.
    validation_behavior :: Maybe Text,
    events :: [MpEvent]
  }
  deriving (Show, Eq, Generic)

instance ToJSON MpCollectReq where
  toJSON = A.genericToJSON A.defaultOptions {A.omitNothingFields = True}

instance FromJSON MpCollectReq where
  parseJSON = A.genericParseJSON A.defaultOptions {A.omitNothingFields = True}

data MpEvent = MpEvent
  { name :: Text,
    params :: A.Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | @/mp/collect@ answers 2xx with an empty body; see 'MpEmptyResponse'.
data MpCollectResp = MpCollectResp
  deriving (Show, Eq, Generic)

instance ToJSON MpCollectResp where
  toJSON _ = A.Null

newtype MpValidationResp = MpValidationResp
  { validationMessages :: [MpValidationMessage]
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON MpValidationResp where
  parseJSON = A.withObject "MpValidationResp" $ \o ->
    MpValidationResp <$> o A..:? "validationMessages" A..!= []

data MpValidationMessage = MpValidationMessage
  { fieldPath :: Maybe Text,
    description :: Text,
    validationCode :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
