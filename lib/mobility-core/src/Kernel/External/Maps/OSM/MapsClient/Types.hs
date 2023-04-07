{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Kernel.External.Maps.OSM.MapsClient.Types where

import Kernel.Prelude

data ReverseGeocodeRes = ReverseGeocodeRes
  { place_id :: Text,
    lat :: Double,
    lon :: Double,
    display_name :: Text,
    address :: AddressInfo
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data AddressInfo = AddressInfo
  { road :: Text,
    neighbourhood :: Text,
    state_district :: Text,
    state :: Text,
    postcode :: Text,
    country :: Text,
    country_code :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
