{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Beckn.Location where

import Data.OpenApi hiding (name)
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.City as City
import qualified Kernel.Types.Beckn.Country as Country
import Kernel.Utils.GenericPretty

data Location = Location
  { country :: CountryV2,
    city :: CityV2
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, PrettyShow)

newtype CountryV2 = CountryV2
  { --name :: Text,
    code :: Country.Country
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, PrettyShow)

newtype CityV2 = CityV2
  { --name :: Text,
    code :: City.City
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, PrettyShow)
