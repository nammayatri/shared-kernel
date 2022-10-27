module Beckn.External.Maps.HasCoordinates where

import qualified Beckn.External.Maps.Types as Types
import Beckn.Prelude

class HasCoordinates a where
  getCoordinates :: a -> Types.LatLong
  default getCoordinates :: (HasField "lat" a Double, HasField "lon" a Double) => a -> Types.LatLong
  getCoordinates = getCoordinatessDefault

getCoordinatessDefault :: (HasField "lat" a Double, HasField "lon" a Double) => a -> Types.LatLong
getCoordinatessDefault loc = Types.LatLong loc.lat loc.lon

instance HasCoordinates Types.LatLong where
  getCoordinates = identity
