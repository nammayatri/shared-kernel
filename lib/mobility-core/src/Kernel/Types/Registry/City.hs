 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Registry.City (City (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)

data City = City
  { name :: Maybe Text,
    code :: Maybe Text
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON City where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON City where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
