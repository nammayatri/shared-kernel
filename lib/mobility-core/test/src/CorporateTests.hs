{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module CorporateTests where

import EulerHS.Prelude
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Types.CorporateTypes
import Kernel.Types.Price (HighPrecMoney (..))
import Kernel.Utils.CorporateBilling
import Kernel.Utils.CorporateRouting
import Kernel.Utils.GenderSafetyRules
import Kernel.Utils.StateMachine
import Test.Tasty
import Test.Tasty.HUnit

-- ==================== State Machine Tests ====================

data BookingState = PENDING | CONFIRMED | IN_PROGRESS | COMPLETED | CANCELLED
  deriving (Eq, Show, Enum, Bounded)

instance StateMachine BookingState where
  validTransitions PENDING = [CONFIRMED, CANCELLED]
  validTransitions CONFIRMED = [IN_PROGRESS, CANCELLED]
  validTransitions IN_PROGRESS = [COMPLETED, CANCELLED]
  validTransitions COMPLETED = []
  validTransitions CANCELLED = []

  isTerminal COMPLETED = True
  isTerminal CANCELLED = True
  isTerminal _ = False

  initialState = PENDING

stateMachineTests :: TestTree
stateMachineTests =
  testGroup
    "StateMachine"
    [ testCase "Valid transition succeeds" $ do
        let result = transition PENDING CONFIRMED
        result @?= TransitionOk CONFIRMED,
      testCase "Invalid transition fails" $ do
        let result = transition PENDING COMPLETED
        result @?= InvalidTransition PENDING COMPLETED [CONFIRMED, CANCELLED],
      testCase "Terminal state has no transitions" $ do
        let result = transition COMPLETED PENDING
        result @?= InvalidTransition COMPLETED PENDING [],
      testCase "Validate valid sequence" $ do
        let results = validateSequence [PENDING, CONFIRMED, IN_PROGRESS, COMPLETED]
        length results @?= 3
        all isOk results @?= True,
      testCase "Validate invalid sequence stops at first error" $ do
        let results = validateSequence [PENDING, COMPLETED, IN_PROGRESS]
        length results @?= 1
        case results of
          (r : _) -> isOk r @?= False
          _ -> assertFailure "Expected at least one result"
    ]
  where
    isOk (TransitionOk _) = True
    isOk _ = False

-- ==================== Routing Tests ====================

sampleStops :: [RouteStop]
sampleStops =
  [ RouteStop "s1" (LatLong 12.9716 77.5946) 1 "emp1" "MALE" PICKUP,
    RouteStop "s2" (LatLong 12.9816 77.5946) 2 "emp2" "FEMALE" PICKUP,
    RouteStop "s3" (LatLong 12.9716 77.6046) 3 "emp3" "MALE" PICKUP,
    RouteStop "s4" (LatLong 12.9616 77.5846) 4 "emp4" "FEMALE" PICKUP,
    RouteStop "s5" (LatLong 12.9916 77.6146) 5 "emp5" "MALE" PICKUP
  ]

routingTests :: TestTree
routingTests =
  testGroup
    "CorporateRouting"
    [ testCase "Cluster stops respects capacity" $ do
        let clusters = clusterStops 2 sampleStops
        all (\c -> length c <= 3) clusters @?= True
        sum (map length clusters) @?= length sampleStops,
      testCase "Empty stops cluster returns empty" $ do
        let clusters = clusterStops 3 ([] :: [RouteStop])
        clusters @?= [],
      testCase "Optimize sequence returns all stops" $ do
        let depot = LatLong 12.9500 77.5800
            optimized = optimizeStopSequence depot sampleStops
        length optimized @?= length sampleStops,
      testCase "Split into routes respects capacity" $ do
        let routes = splitIntoRoutes 2 sampleStops
        all (\r -> length r <= 2) routes @?= True
        sum (map length routes) @?= length sampleStops,
      testCase "Gender constraints applied in night shift" $ do
        let constraints = GenderConstraints True True True
            femaleFirstStops =
              [ RouteStop "s1" (LatLong 12.97 77.59) 1 "e1" "FEMALE" PICKUP,
                RouteStop "s2" (LatLong 12.98 77.59) 2 "e2" "MALE" PICKUP,
                RouteStop "s3" (LatLong 12.96 77.58) 3 "e3" "FEMALE" PICKUP
              ]
            reordered = applyGenderConstraints constraints femaleFirstStops
        case reordered of
          (first : _) -> first.gender @?= "MALE"
          _ -> assertFailure "Reordered list should not be empty",
      testCase "Gender constraints not applied in day shift" $ do
        let constraints = GenderConstraints False True True
            femaleFirstStops =
              [ RouteStop "s1" (LatLong 12.97 77.59) 1 "e1" "FEMALE" PICKUP,
                RouteStop "s2" (LatLong 12.98 77.59) 2 "e2" "MALE" PICKUP
              ]
            reordered = applyGenderConstraints constraints femaleFirstStops
        case reordered of
          (first : _) -> first.gender @?= "FEMALE"
          _ -> assertFailure "List should not be empty"
    ]

-- ==================== Billing Tests ====================

mkConfig :: CorporateBillingModel -> CorporateFareConfig
mkConfig model =
  CorporateFareConfig
    { billingModel = model,
      negotiatedRate = Nothing,
      ratePerSeatKm = Nothing,
      flatRate = Nothing,
      baseRate = Nothing,
      perKmRate = Nothing,
      surgeCap = Nothing,
      corporateDiscountPct = Nothing
    }

mkInput :: CorporateFareInput
mkInput =
  CorporateFareInput
    { actualFare = HighPrecMoney 500,
      distanceKm = 10.0,
      seats = 1,
      surgeMultiplier = Nothing
    }

billingTests :: TestTree
billingTests =
  testGroup
    "CorporateBilling"
    [ testCase "PER_TRIP uses actual fare" $ do
        let config = mkConfig PER_TRIP
            result = calculateCorporateFare config mkInput
        result.finalFare @?= HighPrecMoney 500,
      testCase "PER_TRIP uses negotiated rate when present" $ do
        let config = (mkConfig PER_TRIP) {negotiatedRate = Just (HighPrecMoney 400)}
            result = calculateCorporateFare config mkInput
        result.finalFare @?= HighPrecMoney 400
        result.policyOverride @?= True,
      testCase "PER_SEAT_KM calculates seats * distance * rate" $ do
        let config = (mkConfig PER_SEAT_KM) {ratePerSeatKm = Just (HighPrecMoney 10)}
            input = mkInput {seats = 3, distanceKm = 5.0}
            result = calculateCorporateFare config input
        result.distanceFare @?= HighPrecMoney (toRational (15.0 :: Double) * 10),
      testCase "FLAT_ROUTE uses flat rate" $ do
        let config = (mkConfig FLAT_ROUTE) {flatRate = Just (HighPrecMoney 300)}
            result = calculateCorporateFare config mkInput
        result.finalFare @?= HighPrecMoney 300,
      testCase "HYBRID combines base + per-km" $ do
        let config = (mkConfig HYBRID) {baseRate = Just (HighPrecMoney 100), perKmRate = Just (HighPrecMoney 20)}
            input = mkInput {distanceKm = 5.0}
            result = calculateCorporateFare config input
        result.baseFare @?= HighPrecMoney 100
        result.distanceFare @?= HighPrecMoney (toRational (5.0 :: Double) * 20),
      testCase "PER_EMPLOYEE_MONTH returns actual fare" $ do
        let config = mkConfig PER_EMPLOYEE_MONTH
            result = calculateCorporateFare config mkInput
        result.finalFare @?= HighPrecMoney 500,
      testCase "Surge is capped when exceeding cap" $ do
        let config = (mkConfig PER_TRIP) {surgeCap = Just 1.5}
            input = mkInput {surgeMultiplier = Just 2.0}
            result = calculateCorporateFare config input
        result.surgeApplied @?= True
        result.surgeCapped @?= True,
      testCase "Validate billing entry - model not allowed" $ do
        let policy = CorporatePolicy Nothing Nothing Nothing [PER_TRIP] True
            result = validateBillingEntry policy PER_SEAT_KM (HighPrecMoney 100) 5.0 Nothing
        result @?= BillingModelNotAllowed PER_SEAT_KM,
      testCase "Validate billing entry - fare exceeds limit" $ do
        let policy = CorporatePolicy (Just (HighPrecMoney 200)) Nothing Nothing [PER_TRIP] True
            result = validateBillingEntry policy PER_TRIP (HighPrecMoney 500) 5.0 Nothing
        result @?= FareExceedsLimit (HighPrecMoney 500) (HighPrecMoney 200),
      testCase "Validate billing entry - valid" $ do
        let policy = CorporatePolicy (Just (HighPrecMoney 1000)) Nothing Nothing [PER_TRIP] True
            result = validateBillingEntry policy PER_TRIP (HighPrecMoney 500) 5.0 Nothing
        result @?= BillingValid
    ]

-- ==================== Gender Safety Tests ====================

mkStopWithGender :: Text -> Text -> Int -> RouteStopWithGender
mkStopWithGender sid g seqNo =
  RouteStopWithGender
    { stopId = sid,
      location = LatLong (12.97 + fromIntegral seqNo * 0.01) 77.59,
      sequenceNumber = seqNo,
      gender = g,
      direction = PICKUP
    }

genderSafetyTests :: TestTree
genderSafetyTests =
  testGroup
    "GenderSafetyRules"
    [ testCase "Detect female first pickup violation" $ do
        let constraints = GenderConstraints True True True
            stops =
              [ mkStopWithGender "s1" "FEMALE" 1,
                mkStopWithGender "s2" "MALE" 2,
                mkStopWithGender "s3" "MALE" 3
              ]
            violations = validateRouteGenderSafety constraints stops
        length violations @?= 1
        case violations of
          [FemaleFirstPickup sid] -> sid @?= "s1"
          _ -> assertFailure "Expected FemaleFirstPickup violation",
      testCase "Detect female last drop violation" $ do
        let constraints = GenderConstraints True True True
            stops =
              [ mkStopWithGender "s1" "MALE" 1,
                mkStopWithGender "s2" "MALE" 2,
                mkStopWithGender "s3" "FEMALE" 3
              ]
            violations = validateRouteGenderSafety constraints stops
        length violations @?= 1
        case violations of
          [FemaleLastDrop sid] -> sid @?= "s3"
          _ -> assertFailure "Expected FemaleLastDrop violation",
      testCase "No violation in day shift" $ do
        let constraints = GenderConstraints False True True
            stops =
              [ mkStopWithGender "s1" "FEMALE" 1,
                mkStopWithGender "s2" "MALE" 2,
                mkStopWithGender "s3" "FEMALE" 3
              ]
            violations = validateRouteGenderSafety constraints stops
        violations @?= [],
      testCase "Reorder fixes female first pickup" $ do
        let constraints = GenderConstraints True True True
            stops =
              [ mkStopWithGender "s1" "FEMALE" 1,
                mkStopWithGender "s2" "FEMALE" 2,
                mkStopWithGender "s3" "MALE" 3
              ]
            reordered = reorderForGenderSafety constraints stops
        case reordered of
          (first : _) -> first.gender @?= "MALE"
          _ -> assertFailure "Reordered list should not be empty"
        let newViolations = validateRouteGenderSafety constraints reordered
            hasFirstPickupViolation = any isFirstPickup newViolations
        hasFirstPickupViolation @?= False,
      testCase "Detect outlier stop with excessive detour" $ do
        let stops =
              [ mkStopWithGender "s1" "MALE" 1,
                (mkStopWithGender "s2" "MALE" 2) {location = LatLong 20.0 80.0},
                mkStopWithGender "s3" "MALE" 3
              ]
            outliers = detectOutlierStops 1000.0 stops
        length outliers @?= 1
    ]
  where
    isFirstPickup (FemaleFirstPickup _) = True
    isFirstPickup _ = False

-- ==================== Top-level Test Tree ====================

corporateTests :: TestTree
corporateTests =
  testGroup
    "Corporate Commute"
    [ stateMachineTests,
      routingTests,
      billingTests,
      genderSafetyTests
    ]
