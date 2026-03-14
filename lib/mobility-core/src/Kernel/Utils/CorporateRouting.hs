{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.CorporateRouting where

import qualified Data.List as L
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.CorporateTypes

-- | Haversine distance between two points in meters (pure computation).
--   NOTE: This is intentionally kept as a local pure Double-returning variant.
--   Kernel.Utils.CalculateDistance.distanceBetweenInMeters provides the canonical
--   HighPrecMeters version. This function returns raw Double for use in
--   clustering/optimization where HighPrecMeters would add unnecessary overhead.
haversineDistanceMeters :: LatLong -> LatLong -> Double
haversineDistanceMeters (LatLong lat1 lon1) (LatLong lat2 lon2) =
  let r = 6371000.0
      dlat = deg2Rad (lat2 - lat1)
      dlon = deg2Rad (lon2 - lon1)
      rlat1 = deg2Rad lat1
      rlat2 = deg2Rad lat2
      a = sin (dlat / 2) ^ (2 :: Int) + cos rlat1 * cos rlat2 * sin (dlon / 2) ^ (2 :: Int)
   in 2 * r * atan2 (sqrt a) (sqrt (1 - a))
  where
    deg2Rad d = d * pi / 180

-- | Cluster stops into groups of at most `capacity` using simple K-means on geo-location.
--   Returns a list of clusters (each cluster is a list of stops).
clusterStops :: Int -> [RouteStop] -> [[RouteStop]]
clusterStops capacity stops
  | capacity <= 0 = [stops]
  | null stops = []
  | otherwise =
      let k = max 1 ((length stops + capacity - 1) `div` capacity)
          initialCentroids = take k (map (.location) stops)
          finalAssignment = iterateKMeans 20 initialCentroids stops
       in map snd finalAssignment
  where
    iterateKMeans :: Int -> [LatLong] -> [RouteStop] -> [(LatLong, [RouteStop])]
    iterateKMeans 0 centroids ss = assignToClusters centroids ss
    iterateKMeans n centroids ss =
      let clusters = assignToClusters centroids ss
          newCentroids = map (computeCentroid . snd) clusters
       in if newCentroids == centroids
            then clusters
            else iterateKMeans (n - 1) newCentroids ss

    assignToClusters :: [LatLong] -> [RouteStop] -> [(LatLong, [RouteStop])]
    assignToClusters centroids ss =
      let grouped = map (\c -> (c, filter (\s -> nearestCentroid centroids s.location == c) ss)) centroids
       in filter (not . null . snd) grouped

    nearestCentroid :: [LatLong] -> LatLong -> LatLong
    nearestCentroid [] loc = loc
    nearestCentroid cs loc = L.minimumBy (\a b -> compare (haversineDistanceMeters loc a) (haversineDistanceMeters loc b)) cs

    computeCentroid :: [RouteStop] -> LatLong
    computeCentroid [] = LatLong 0 0
    computeCentroid ss =
      let n = fromIntegral (length ss)
          avgLat = sum (map ((.lat) . (.location)) ss) / n
          avgLon = sum (map ((.lon) . (.location)) ss) / n
       in LatLong avgLat avgLon

-- | Nearest-neighbor heuristic for ordering stops starting from a depot location.
optimizeStopSequence :: LatLong -> [RouteStop] -> [RouteStop]
optimizeStopSequence _ [] = []
optimizeStopSequence depot stops = go depot stops []
  where
    go _ [] acc = reverse acc
    go currentLoc remaining acc =
      let nearest = L.minimumBy (\a b -> compare (haversineDistanceMeters currentLoc a.location) (haversineDistanceMeters currentLoc b.location)) remaining
          rest = filter (\s -> s.stopId /= nearest.stopId) remaining
       in go nearest.location rest (nearest : acc)

-- | Apply gender constraints: ensure no female is first pickup or last drop when isNightShift.
applyGenderConstraints :: GenderConstraints -> [RouteStop] -> [RouteStop]
applyGenderConstraints constraints stops
  | not constraints.isNightShift = stops
  | null stops = stops
  | otherwise =
      let withFirstPickupFixed =
            if constraints.noFemaleFirstPickup
              then ensureNotFemaleFirst stops
              else stops
          withLastDropFixed =
            if constraints.noFemaleLastDrop
              then ensureNotFemaleLast withFirstPickupFixed
              else withFirstPickupFixed
       in withLastDropFixed
  where
    ensureNotFemaleFirst :: [RouteStop] -> [RouteStop]
    ensureNotFemaleFirst [] = []
    ensureNotFemaleFirst allStops@(first : _rest)
      | first.gender /= "FEMALE" = allStops
      | otherwise =
          case L.findIndex (\s -> s.gender /= "FEMALE") allStops of
            Nothing -> allStops
            Just idx ->
              let nonFemale = allStops !! idx
                  remaining = take idx allStops <> drop (idx + 1) allStops
               in nonFemale : remaining

    ensureNotFemaleLast :: [RouteStop] -> [RouteStop]
    ensureNotFemaleLast [] = []
    ensureNotFemaleLast allStops
      | (L.last allStops).gender /= "FEMALE" = allStops
      | otherwise =
          case L.findIndex (\s -> s.gender /= "FEMALE") (reverse allStops) of
            Nothing -> allStops
            Just idx ->
              let realIdx = length allStops - 1 - idx
                  nonFemale = allStops !! realIdx
                  remaining = take realIdx allStops <> drop (realIdx + 1) allStops
               in remaining <> [nonFemale]

-- | Split stops into routes of at most the given vehicle capacity.
splitIntoRoutes :: Int -> [RouteStop] -> [[RouteStop]]
splitIntoRoutes _ [] = []
splitIntoRoutes cap stops
  | cap <= 0 = [stops]
  | otherwise = go stops
  where
    go [] = []
    go ss = take cap ss : go (drop cap ss)
