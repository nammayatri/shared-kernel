{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Kernel.External.Maps.NextBillion.Types
  ( module Kernel.External.Maps.NextBillion.Types,
  )
where

import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude

data DirectionsResp = DirectionsResp
  { routes :: [Route],
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

data Route = Route
  { geometry :: Text,
    distance :: Double, -- meters
    duration :: Double -- seconds
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

data GetRoutesRequest = GetRoutesRequest
  { waypoints :: NonEmpty LatLong,
    alternatives :: Maybe Bool,
    altcount :: Maybe Int,
    routeType :: Maybe Text,
    option :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)
