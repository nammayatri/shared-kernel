{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.HasCoordinates where

import qualified Kernel.External.Maps.Types as Types
import Kernel.Prelude

class HasCoordinates a where
  getCoordinates :: a -> Types.LatLong
  default getCoordinates :: (HasField "lat" a Double, HasField "lon" a Double) => a -> Types.LatLong
  getCoordinates = getCoordinatessDefault

getCoordinatessDefault :: (HasField "lat" a Double, HasField "lon" a Double) => a -> Types.LatLong
getCoordinatessDefault loc = Types.LatLong loc.lat loc.lon

instance HasCoordinates Types.LatLong where
  getCoordinates = identity

class HasDriverId a where
  getDriverId :: a -> Maybe Text
  default getDriverId :: (HasField "driverId" a (Maybe Text)) => a -> Maybe Text
  getDriverId = getDriverIdDefault

  getCoordinatesAndDriverId :: (HasCoordinates a, HasDriverId a) => a -> Text
  getCoordinatesAndDriverId req = show (getCoordinates req) <> "|" <> fromMaybe "unknown" (getDriverId req)

getDriverIdDefault :: (HasField "driverId" a (Maybe Text)) => a -> Maybe Text
getDriverIdDefault req = req.driverId

instance HasDriverId Types.LatLong where
  getDriverId = const Nothing
