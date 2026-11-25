{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module OsrmRoadTierTraffic where

import qualified Data.Map.Strict as M
import EulerHS.Prelude
import Kernel.External.Maps.Interface.OSRM (classifySpeed, legTiers, mkTrafficSegments)
import Kernel.External.Maps.Interface.Types (RouteTrafficSegment (..), TrafficSpeed (..))
import Kernel.External.Maps.OSRM.Config
  ( OSRMTrafficConfig (..),
    OSRMTrafficThresholds (..),
    RoadTier (..),
  )
import qualified Kernel.External.Maps.OSRM.RoadsClient as OSRM
import Kernel.External.Maps.Types (LatLong (..))
import Test.Tasty
import Test.Tasty.HUnit

cfg :: OSRMTrafficConfig
cfg =
  OSRMTrafficConfig
    { jamBelowKmph = 5,
      slowBelowKmph = 15,
      minSegmentLengthMeters = 50,
      tierThresholds =
        Just . M.fromList $
          [ (Main, OSRMTrafficThresholds {jamBelowKmph = 40, slowBelowKmph = 60}),
            (Residential, OSRMTrafficThresholds {jamBelowKmph = 5, slowBelowKmph = 12})
          ]
    }

-- | Distinct points; an intersection must sit exactly on one of its step's coordinates,
-- as it always does in a real OSRM response.
point :: Int -> OSRM.Location
point n = OSRM.Location $ LatLong (12.9 + fromIntegral n / 1000) (77.5 + fromIntegral n / 1000)

step :: [Int] -> Maybe [(Int, Maybe [Text])] -> OSRM.Steps
step coords mbIntersections =
  OSRM.Steps
    { geometry = OSRM.RouteGeometry {coordinates = map point coords},
      maneuver = OSRM.Maneuver {location = point firstCoord},
      intersections = map junction <$> mbIntersections
    }
  where
    firstCoord = case coords of
      (c : _) -> c
      [] -> 0
    junction (at, classes) = OSRM.Intersection {location = point at, classes}

-- | OSRM always ends a leg with @arrive@: two identical coordinates owning no segment.
arriveStep :: Int -> OSRM.Steps
arriveStep at = step [at, at] (Just [(at, Just ["main"])])

leg :: [OSRM.Steps] -> Maybe [Double] -> OSRM.RouteResponseLeg
leg steps mbSpeeds =
  OSRM.RouteResponseLeg
    { steps,
      distance = 0,
      duration = 0,
      weight = 0,
      annotation =
        mbSpeeds <&> \speeds ->
          OSRM.RouteAnnotation
            { speed = Just speeds,
              distance = Just $ replicate (length speeds) 100,
              duration = Nothing
            }
    }

-- 30 km/h is 8.33 m/s: a jam on a highway, normal on a residential street.
thirtyKmph :: Double
thirtyKmph = 30 / 3.6

tierPicksItsOwnThresholds :: TestTree
tierPicksItsOwnThresholds =
  testCase "each tier is judged by its own thresholds, and an untiered road by the config's" $ do
    classifySpeed cfg (Just Main) thirtyKmph @?= TrafficJam
    classifySpeed cfg (Just Residential) thirtyKmph @?= TrafficNormal
    -- Arterial has no entry in tierThresholds, so it falls back like an untiered road
    classifySpeed cfg (Just Arterial) thirtyKmph @?= TrafficNormal
    classifySpeed cfg Nothing thirtyKmph @?= TrafficNormal

tierHoldsUntilTheNextJunction :: TestTree
tierHoldsUntilTheNextJunction =
  testCase "a junction's tier covers the segments up to the next junction, the last to the step's end" $ do
    let oneStep = step [0, 1, 2, 3, 4] (Just [(0, Just ["main"]), (2, Just ["residential"])])
    legTiers (leg [oneStep, arriveStep 4] Nothing)
      @?= [Just Main, Just Main, Just Residential, Just Residential]

arriveStepOwnsNoSegment :: TestTree
arriveStepOwnsNoSegment =
  testCase "the arrive step adds no tier, so tiers line up with annotation.speed" $ do
    let steps = [step [0, 1, 2] (Just [(0, Just ["main"])]), arriveStep 2]
        speeds = [thirtyKmph, thirtyKmph]
    length (legTiers (leg steps Nothing)) @?= length speeds
    legTiers (leg steps Nothing) @?= [Just Main, Just Main]

nonTierClassesAreIgnored :: TestTree
nonTierClassesAreIgnored =
  testCase "a non-tier class, or none at all, leaves the segment untiered" $ do
    let steps =
          [ step
              [0, 1, 2, 3]
              ( Just
                  [ (0, Just ["main", "toll"]), -- toll is not a tier; main still wins
                    (1, Just ["ferry"]), -- no tier among the classes
                    (2, Nothing) -- no classes key at all
                  ]
              ),
            arriveStep 3
          ]
    legTiers (leg steps Nothing) @?= [Just Main, Nothing, Nothing]

misalignedTiersFallBackInsteadOfShifting :: TestTree
misalignedTiersFallBackInsteadOfShifting =
  testCase "when tiers cannot line up with the speeds, the leg uses the untiered thresholds" $ do
    -- One segment of geometry but four speeds: tiering must be abandoned for the leg, not
    -- applied to the wrong segments. 30 km/h is then normal (untiered slowBelow is 15).
    let steps = [step [0, 1] (Just [(0, Just ["main"])]), arriveStep 1]
        misaligned = leg steps (Just $ replicate 4 thirtyKmph)
    mkTrafficSegments (Just cfg) [misaligned] @?= Just []
    let aligned = leg [step [0, 1, 2, 3, 4] (Just [(0, Just ["main"])]), arriveStep 4] (Just $ replicate 4 thirtyKmph)
    mkTrafficSegments (Just cfg) [aligned]
      @?= Just [RouteTrafficSegment {startIndex = 0, endIndex = 4, speed = TrafficJam}]

segmentsAreHalfOpenAndSkipNormal :: TestTree
segmentsAreHalfOpenAndSkipNormal =
  testCase "only non-normal runs are reported, as half-open index spans" $ do
    let steps =
          [ step
              [0, 1, 2, 3, 4]
              (Just [(0, Just ["residential"]), (2, Just ["main"])]),
            arriveStep 4
          ]
        oneLeg = leg steps (Just $ replicate 4 thirtyKmph)
    mkTrafficSegments (Just cfg) [oneLeg]
      @?= Just [RouteTrafficSegment {startIndex = 2, endIndex = 4, speed = TrafficJam}]
    mkTrafficSegments Nothing [oneLeg] @?= Nothing

osrmRoadTierTrafficTests :: TestTree
osrmRoadTierTrafficTests =
  testGroup
    "OSRM road-tier traffic"
    [ tierPicksItsOwnThresholds,
      tierHoldsUntilTheNextJunction,
      arriveStepOwnsNoSegment,
      nonTierClassesAreIgnored,
      misalignedTiersFallBackInsteadOfShifting,
      segmentsAreHalfOpenAndSkipNormal
    ]
