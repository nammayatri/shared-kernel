{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Kernel.Types.Beckn.Error where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Kernel.Utils.GenericPretty (PrettyShow, Showable (Showable))
import Kernel.Utils.JSON

data Error = Error
  { _type :: ErrorType,
    code :: Text,
    path :: Maybe Text,
    message :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToSchema, PrettyShow)

data ErrorType
  = CONTEXT_ERROR
  | CORE_ERROR
  | INTERNAL_ERROR -- Not a spec value. TODO: get rid of it.
  | DOMAIN_ERROR
  | POLICY_ERROR
  | JSON_SCHEMA_ERROR
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSchema)
  deriving (PrettyShow) via Showable ErrorType

instance FromJSON ErrorType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON ErrorType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON Error where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Error where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
