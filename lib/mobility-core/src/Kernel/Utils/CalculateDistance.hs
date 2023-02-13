module Kernel.Utils.CalculateDistance where

import EulerHS.Prelude hiding (id, state)
import Kernel.External.Maps.Types
import Kernel.Prelude (atan2)
import Kernel.Types.Common (HighPrecMeters (..))

distanceBetweenInMeters :: LatLong -> LatLong -> HighPrecMeters
distanceBetweenInMeters (LatLong lat1 lon1) (LatLong lat2 lon2) =
  -- Calculating using haversine formula
  let r = 6371000 -- Radius of earth in meters
      dlat = deg2Rad $ lat2 - lat1
      dlon = deg2Rad $ lon2 - lon1
      rlat1 = deg2Rad lat1
      rlat2 = deg2Rad lat2
      sq x = x * x
      -- Calculated distance is real (not imaginary) when 0 <= h <= 1
      -- Ideally in our use case h wouldn't go out of bounds
      h = sq (sin (dlat / 2)) + cos rlat1 * cos rlat2 * sq (sin (dlon / 2))
   in realToFrac $ 2 * r * atan2 (sqrt h) (sqrt (1 - h))

deg2Rad :: Double -> Double
deg2Rad degree = degree * pi / 180

getRouteLinearLength :: [LatLong] -> HighPrecMeters
getRouteLinearLength pts@(_ : t) = sum $ zipWith distanceBetweenInMeters pts t
getRouteLinearLength _ = 0
