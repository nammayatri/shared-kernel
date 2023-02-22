 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DerivingVia #-}

module Kernel.Types.Beckn.Domain (Domain (..)) where

import Data.Aeson
import Data.OpenApi hiding (Example)
import EulerHS.Prelude
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.Example
import Kernel.Utils.GenericPretty
import Kernel.Utils.JSON (replaceUnderscoresString)

data Domain
  = MOBILITY
  | LOCAL_RETAIL
  | FOOD_AND_BEVERAGE
  | HEALTHCARE
  | METRO
  | PARKING
  | PUBLIC_TRANSPORT
  | LOGISTICS
  deriving (Eq, Generic, Show, Read, FromDhall)
  deriving (PrettyShow) via Showable Domain

instance Example Domain where
  example = MOBILITY

customAesonOptions :: Options
customAesonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "MOBILITY" -> "nic2004:60221"
        "LOCAL_RETAIL" -> "nic2004:52110"
        "METRO" -> "nic2004:60212"
        "PARKING" -> "nic2004:63031"
        "PUBLIC_TRANSPORT" -> "nic2004:63032"
        "LOGISTICS" -> "nic2004:60232"
        val -> replaceUnderscoresString val, -- TODO: update remaining domains with codes
      sumEncoding = UntaggedValue
    }

instance ToSchema Domain where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions customAesonOptions

instance ToJSON Domain where
  toJSON = genericToJSON customAesonOptions

instance FromJSON Domain where
  parseJSON = genericParseJSON customAesonOptions
