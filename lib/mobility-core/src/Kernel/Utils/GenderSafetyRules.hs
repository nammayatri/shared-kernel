{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.GenderSafetyRules where

import qualified Data.List as L
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.CorporateTypes (GenderConstraints (..), RouteDirection (..), RouteStop (..))
import Kernel.Utils.CorporateRouting (haversineDistanceMeters)

-- | A route stop annotated with gender for safety analysis
data RouteStopWithGender = RouteStopWithGender
  { stopId :: Text,
    location :: LatLong,
    sequenceNumber :: Int,
    gender :: Text,
    direction :: RouteDirection
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Types of gender safety violations
data GenderSafetyViolation
  = FemaleFirstPickup Text
  | FemaleLastDrop Text
  | OutlierStopExcessiveDetour Text Double Double
  deriving (Eq, Show)

-- | Convert a RouteStop to RouteStopWithGender
fromRouteStop :: RouteStop -> RouteStopWithGender
fromRouteStop rs =
  RouteStopWithGender
    { stopId = rs.stopId,
      location = rs.location,
      sequenceNumber = rs.sequenceNumber,
      gender = rs.gender,
      direction = rs.direction
    }

-- | Validate a route for gender safety violations during night shifts.
validateRouteGenderSafety :: GenderConstraints -> [RouteStopWithGender] -> [GenderSafetyViolation]
validateRouteGenderSafety constraints stops
  | not constraints.isNightShift = []
  | null stops = []
  | otherwise =
      let firstViolation =
            if constraints.noFemaleFirstPickup
              then checkFirstPickup stops
              else []
          lastViolation =
            if constraints.noFemaleLastDrop
              then checkLastDrop stops
              else []
       in firstViolation <> lastViolation
  where
    checkFirstPickup [] = []
    checkFirstPickup (s : _)
      | s.gender == "FEMALE" = [FemaleFirstPickup s.stopId]
      | otherwise = []

    checkLastDrop [] = []
    checkLastDrop ss
      | (L.last ss).gender == "FEMALE" = [FemaleLastDrop (L.last ss).stopId]
      | otherwise = []

-- | Reorder stops to comply with gender safety rules.
--   Moves female stops away from first pickup and last drop positions.
reorderForGenderSafety :: GenderConstraints -> [RouteStopWithGender] -> [RouteStopWithGender]
reorderForGenderSafety constraints stops
  | not constraints.isNightShift = stops
  | null stops = stops
  | otherwise =
      let withFirstFixed =
            if constraints.noFemaleFirstPickup
              then fixFirstPickup stops
              else stops
          withLastFixed =
            if constraints.noFemaleLastDrop
              then fixLastDrop withFirstFixed
              else withFirstFixed
       in renumber withLastFixed
  where
    fixFirstPickup :: [RouteStopWithGender] -> [RouteStopWithGender]
    fixFirstPickup [] = []
    fixFirstPickup allStops@(first : _rest)
      | first.gender /= "FEMALE" = allStops
      | otherwise =
          case L.findIndex (\s -> s.gender /= "FEMALE") allStops of
            Nothing -> allStops
            Just idx ->
              let nonFemale = allStops !! idx
                  remaining = take idx allStops <> drop (idx + 1) allStops
               in nonFemale : remaining

    fixLastDrop :: [RouteStopWithGender] -> [RouteStopWithGender]
    fixLastDrop [] = []
    fixLastDrop allStops
      | (L.last allStops).gender /= "FEMALE" = allStops
      | otherwise =
          case L.findIndex (\s -> s.gender /= "FEMALE") (reverse allStops) of
            Nothing -> allStops
            Just idx ->
              let realIdx = length allStops - 1 - idx
                  nonFemale = allStops !! realIdx
                  remaining = take realIdx allStops <> drop (realIdx + 1) allStops
               in remaining <> [nonFemale]

    renumber :: [RouteStopWithGender] -> [RouteStopWithGender]
    renumber = zipWith (\i s -> s {sequenceNumber = i}) [1 ..]

-- | Detect stops that add excessive detour to the route.
--   A stop is an outlier if removing it reduces total route distance by more than the threshold.
detectOutlierStops :: Double -> [RouteStopWithGender] -> [GenderSafetyViolation]
detectOutlierStops thresholdMeters stops
  | length stops < 3 = []
  | otherwise = concatMap checkStop [1 .. length stops - 2]
  where
    totalDistance = routeDistance stops

    checkStop idx =
      let without = take idx stops <> drop (idx + 1) stops
          distWithout = routeDistance without
          saving = totalDistance - distWithout
          s = stops !! idx
       in if saving > thresholdMeters
            then [OutlierStopExcessiveDetour s.stopId saving thresholdMeters]
            else []

    routeDistance :: [RouteStopWithGender] -> Double
    routeDistance ss = sum $ zipWith (\a b -> haversineDistanceMeters a.location b.location) ss (drop 1 ss)
