{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Utils.ComputeIntersection where

import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Utils.GenericPretty (PrettyShow)

type RoutePoints = [LatLong]

data BoundingBox = BoundingBox
  { topLeft :: LatLong,
    topRight :: LatLong,
    bottomLeft :: LatLong,
    bottomRight :: LatLong
  }
  deriving (Generic, Eq, Show, Read)

data LineSegment = LineSegment
  { start :: LatLong,
    end :: LatLong
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema, PrettyShow, Ord, Read)

$(mkBeamInstancesForEnumAndList ''LineSegment)

data Orientation = Collinear | Clockwise | AntiClockwise
  deriving (Generic, Eq, Show, Read)

-- Function to calculate cross product of two vectors (PQ x QR)
crossProduct :: LatLong -> LatLong -> LatLong -> Double
crossProduct p q r = (q.lat - p.lat) * (r.lon - q.lon) - (r.lat - q.lat) * (q.lon - p.lon)

-- Function to determine orientation of three points
orientation :: LatLong -> LatLong -> LatLong -> Orientation
orientation p q r =
  let val = crossProduct p q r
   in if val == 0 then Collinear else if val > 0 then Clockwise else AntiClockwise

-- Function to check if point q lies on segment p-r
onSegment :: LatLong -> LatLong -> LatLong -> Bool
onSegment p q r =
  q.lat <= max (p.lat) (r.lat)
    && q.lat >= min (p.lat) (r.lat)
    && q.lon <= max (p.lon) (r.lon)
    && q.lon >= min (p.lon) (r.lon)

-- Function to check if two line segments intersect
doIntersect :: LineSegment -> LineSegment -> Bool
doIntersect (LineSegment p1 q1) (LineSegment p2 q2) =
  let o1 = orientation p1 q1 p2
      o2 = orientation p1 q1 q2
      o3 = orientation p2 q2 p1
      o4 = orientation p2 q2 q1
   in (o1 /= o2 && o3 /= o4)
        || (o1 == Collinear && onSegment p1 p2 q1)
        || (o2 == Collinear && onSegment p1 q2 q1)
        || (o3 == Collinear && onSegment p2 p1 q2)
        || (o4 == Collinear && onSegment p2 q1 q2)

-- Check if a point lies within a bounding box
pointWithinBoundingBox :: LatLong -> BoundingBox -> Bool
pointWithinBoundingBox (LatLong lat lon) (BoundingBox (LatLong topLeftLat topLeftLon) (LatLong topRightLat topRightLon) (LatLong bottomLeftLat bottomLeftLon) (LatLong bottomRightLat bottomRightLon)) =
  lat <= max topLeftLat topRightLat && lat >= min bottomLeftLat bottomRightLat && lon >= min topLeftLon bottomLeftLon && lon <= max topRightLon bottomRightLon

-- Check if a line segment is within the bounding box
lineSegmentWithinBoundingBox :: LineSegment -> BoundingBox -> Bool
lineSegmentWithinBoundingBox (LineSegment startPoint endPoint) boundingBox =
  pointWithinBoundingBox startPoint boundingBox && pointWithinBoundingBox endPoint boundingBox

-- Check if any line between two route points passes through intersection points
doRouteIntersectWithLine :: RoutePoints -> LineSegment -> Bool
doRouteIntersectWithLine [] _ = False
doRouteIntersectWithLine [_] _ = False
doRouteIntersectWithLine (p1 : p2 : ps) line =
  doIntersect (LineSegment p1 p2) line || doRouteIntersectWithLine (p2 : ps) line

getBoundingBox :: RoutePoints -> BoundingBox
getBoundingBox points =
  let lats = map lat points
      lons = map lon points
      minLat = minimum lats
      maxLat = maximum lats
      minLon = minimum lons
      maxLon = maximum lons
      topLeftPoint = LatLong maxLat minLon
      topRightPoint = LatLong maxLat maxLon
      bottomLeftPoint = LatLong minLat minLon
      bottomRightPoint = LatLong minLat maxLon
   in BoundingBox topLeftPoint topRightPoint bottomLeftPoint bottomRightPoint

-- Check if intersection points lie within a bounding box and if any line between two route points passes through the intersection line
checkIntersection :: RoutePoints -> LineSegment -> Bool
checkIntersection [] _ = False
checkIntersection [_] _ = False
checkIntersection points intersectionLine = do
  lineSegmentWithinBoundingBox intersectionLine (getBoundingBox points)
    && doRouteIntersectWithLine points intersectionLine
