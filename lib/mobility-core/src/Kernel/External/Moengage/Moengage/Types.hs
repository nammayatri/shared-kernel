{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Moengage.Moengage.Types where

import Data.Aeson (Value)
import Kernel.Prelude
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)

data MoengageEventReq = MoengageEventReq
  { _type :: Text,
    customer_id :: Text,
    actions :: [MoengageAction]
  }
  deriving (Show, Generic)

instance ToJSON MoengageEventReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON MoengageEventReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

data MoengageAction = MoengageAction
  { action :: Text,
    attributes :: Value
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data MoengageEventResp = MoengageEventResp
  { status :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)
