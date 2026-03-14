{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module CorporateTests where

import qualified Data.List as L
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
          _ -> assertFailure "Expected at least one result",
      testCase "Validate empty sequence returns empty" $ do
        let results = validateSequence ([] :: [BookingState])
        results @?= [],
      testCase "Validate single state sequence returns empty" $ do
        let results = validateSequence [PENDING]
        results @?= [],
      testCase "Self-transition is invalid for PENDING" $ do
        let result = transition PENDING PENDING
        result @?= InvalidTransition PENDING PENDING [CONFIRMED, CANCELLED],
      testCase "CANCELLED to CANCELLED is invalid" $ do
        let result = transition CANCELLED CANCELLED
        result @?= InvalidTransition CANCELLED CANCELLED [],
      testCase "Multi-step valid path through state machine" $ do
        let results = validateSequence [PENDING, CONFIRMED, CANCELLED]
        length results @?= 2
        all isOk results @?= True,
      -- NEW: Double transition - attempting same valid transition twice in sequence
      testCase "Double transition PENDING->CONFIRMED->CONFIRMED is invalid" $ do
        let results = validateSequence [PENDING, CONFIRMED, CONFIRMED]
        length results @?= 2
        case results of
          [r1, r2] -> do
            isOk r1 @?= True
            isOk r2 @?= False
          _ -> assertFailure "Expected exactly 2 results",
      -- NEW: All terminal states reject all transitions
      testCase "COMPLETED rejects transition to every state" $ do
        let allStates = [PENDING, CONFIRMED, IN_PROGRESS, COMPLETED, CANCELLED]
            results = map (transition COMPLETED) allStates
        all (not . isOk) results @?= True,
      testCase "CANCELLED rejects transition to every state" $ do
        let allStates = [PENDING, CONFIRMED, IN_PROGRESS, COMPLETED, CANCELLED]
            results = map (transition CANCELLED) allStates
        all (not . isOk) results @?= True,
      -- NEW: Reverse path is invalid
      testCase "Reverse path COMPLETED->IN_PROGRESS->CONFIRMED->PENDING is invalid" $ do
        let results = validateSequence [COMPLETED, IN_PROGRESS, CONFIRMED, PENDING]
        length results @?= 1
        case results of
          (r : _) -> isOk r @?= False
          _ -> assertFailure "Expected at least one result",
      -- NEW: Full valid lifecycle path
      testCase "Full lifecycle PENDING->CONFIRMED->IN_PROGRESS->COMPLETED" $ do
        let results = validateSequence [PENDING, CONFIRMED, IN_PROGRESS, COMPLETED]
        length results @?= 3
        all isOk results @?= True,
      -- NEW: Skip state is invalid
      testCase "Skipping CONFIRMED state (PENDING->IN_PROGRESS) is invalid" $ do
        let result = transition PENDING IN_PROGRESS
        isOk result @?= False,
      -- NEW: isTerminal checks
      testCase "isTerminal returns True only for COMPLETED and CANCELLED" $ do
        isTerminal PENDING @?= False
        isTerminal CONFIRMED @?= False
        isTerminal IN_PROGRESS @?= False
        isTerminal COMPLETED @?= True
        isTerminal CANCELLED @?= True,
      -- NEW: initialState check
      testCase "initialState is PENDING" $ do
        (initialState :: BookingState) @?= PENDING,
      -- NEW: Cancellation from every non-terminal state
      testCase "CANCELLED is reachable from all non-terminal states" $ do
        isOk (transition PENDING CANCELLED) @?= True
        isOk (transition CONFIRMED CANCELLED) @?= True
        isOk (transition IN_PROGRESS CANCELLED) @?= True,
      -- NEW: Repeated state sequence
      testCase "Validate sequence with repeated PENDING states" $ do
        let results = validateSequence [PENDING, PENDING, PENDING]
        length results @?= 1
        isOk (head results) @?= False,
      -- NEW: TransitionResult carries correct allowed list
      testCase "InvalidTransition carries correct allowed transitions" $ do
        let result = transition IN_PROGRESS PENDING
        case result of
          InvalidTransition from to allowed -> do
            from @?= IN_PROGRESS
            to @?= PENDING
            allowed @?= [COMPLETED, CANCELLED]
          _ -> assertFailure "Expected InvalidTransition"
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
          _ -> assertFailure "List should not be empty",
      testCase "Single stop returns unchanged" $ do
        let singleStop = [RouteStop "s1" (LatLong 12.97 77.59) 1 "e1" "MALE" PICKUP]
            optimized = optimizeStopSequence (LatLong 12.95 77.58) singleStop
        length optimized @?= 1,
      testCase "Split into routes with empty list returns empty" $ do
        let routes = splitIntoRoutes 3 ([] :: [RouteStop])
        routes @?= [],
      testCase "Cluster with capacity larger than stops returns single cluster" $ do
        let twoStops = take 2 sampleStops
            clusters = clusterStops 10 twoStops
        sum (map length clusters) @?= 2,
      testCase "Split into routes with capacity 1 creates one route per stop" $ do
        let routes = splitIntoRoutes 1 sampleStops
        length routes @?= length sampleStops
        all (\r -> length r == 1) routes @?= True,
      testCase "Gender constraints on empty stops returns empty" $ do
        let constraints = GenderConstraints True True True
            reordered = applyGenderConstraints constraints ([] :: [RouteStop])
        reordered @?= [],
      -- NEW: Zero capacity clusters all stops into one group
      testCase "Cluster with zero capacity returns all stops in one group" $ do
        let clusters = clusterStops 0 sampleStops
        length clusters @?= 1
        sum (map length clusters) @?= length sampleStops,
      -- NEW: Negative capacity behaves like zero capacity
      testCase "Cluster with negative capacity returns all stops in one group" $ do
        let clusters = clusterStops (-5) sampleStops
        length clusters @?= 1,
      -- NEW: Duplicate stops (same location)
      testCase "Optimize sequence handles duplicate locations" $ do
        let depot = LatLong 12.95 77.58
            dupStops =
              [ RouteStop "s1" (LatLong 12.97 77.59) 1 "e1" "MALE" PICKUP,
                RouteStop "s2" (LatLong 12.97 77.59) 2 "e2" "MALE" PICKUP,
                RouteStop "s3" (LatLong 12.98 77.60) 3 "e3" "MALE" PICKUP
              ]
            optimized = optimizeStopSequence depot dupStops
        length optimized @?= 3,
      -- NEW: Split with zero capacity returns all in one group
      testCase "Split with zero capacity returns all stops in one group" $ do
        let routes = splitIntoRoutes 0 sampleStops
        length routes @?= 1
        head routes @?= sampleStops,
      -- NEW: Split with negative capacity returns all in one group
      testCase "Split with negative capacity returns all stops in one group" $ do
        let routes = splitIntoRoutes (-1) sampleStops
        length routes @?= 1,
      -- NEW: Optimize preserves stop IDs
      testCase "Optimize sequence preserves all stop IDs" $ do
        let depot = LatLong 12.95 77.58
            optimized = optimizeStopSequence depot sampleStops
            originalIds = L.sort $ map (.stopId) sampleStops
            optimizedIds = L.sort $ map (.stopId) optimized
        optimizedIds @?= originalIds,
      -- NEW: All stops at same location
      testCase "All stops at identical location cluster into single cluster" $ do
        let identicalStops =
              [ RouteStop "s1" (LatLong 12.97 77.59) 1 "e1" "MALE" PICKUP,
                RouteStop "s2" (LatLong 12.97 77.59) 2 "e2" "MALE" PICKUP,
                RouteStop "s3" (LatLong 12.97 77.59) 3 "e3" "MALE" PICKUP
              ]
            clusters = clusterStops 10 identicalStops
        sum (map length clusters) @?= 3,
      -- NEW: Gender constraints - all male stops, night shift
      testCase "All male stops with night shift constraints returns unchanged order" $ do
        let constraints = GenderConstraints True True True
            maleStops =
              [ RouteStop "s1" (LatLong 12.97 77.59) 1 "e1" "MALE" PICKUP,
                RouteStop "s2" (LatLong 12.98 77.59) 2 "e2" "MALE" PICKUP
              ]
            reordered = applyGenderConstraints constraints maleStops
        reordered @?= maleStops,
      -- NEW: Gender constraints only for noFemaleLastDrop
      testCase "Gender constraint only noFemaleLastDrop reorders last but not first" $ do
        let constraints = GenderConstraints True False True
            stops =
              [ RouteStop "s1" (LatLong 12.97 77.59) 1 "e1" "FEMALE" PICKUP,
                RouteStop "s2" (LatLong 12.98 77.59) 2 "e2" "MALE" PICKUP,
                RouteStop "s3" (LatLong 12.96 77.58) 3 "e3" "FEMALE" PICKUP
              ]
            reordered = applyGenderConstraints constraints stops
        -- First should remain FEMALE since noFemaleFirstPickup is False
        case reordered of
          (first : _) -> first.gender @?= "FEMALE"
          _ -> assertFailure "Should have stops"
        -- Last should not be FEMALE
        case reverse reordered of
          (lst : _) -> lst.gender @?= "MALE"
          _ -> assertFailure "Should have stops"
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
        result @?= BillingValid,
      testCase "Validate billing entry - distance exceeds limit" $ do
        let policy = CorporatePolicy Nothing (Just 10.0) Nothing [PER_TRIP] True
            result = validateBillingEntry policy PER_TRIP (HighPrecMoney 500) 15.0 Nothing
        result @?= DistanceExceedsLimit 15.0 10.0,
      testCase "Validate billing entry - surge not allowed" $ do
        let policy = CorporatePolicy Nothing Nothing Nothing [PER_TRIP] False
            result = validateBillingEntry policy PER_TRIP (HighPrecMoney 500) 5.0 (Just 1.5)
        result @?= SurgeNotAllowed,
      testCase "Surge multiplier <= 1.0 is not applied" $ do
        let config = (mkConfig PER_TRIP) {surgeCap = Just 2.0}
            input = mkInput {surgeMultiplier = Just 1.0}
            result = calculateCorporateFare config input
        result.surgeApplied @?= False
        result.surgeCapped @?= False,
      testCase "Surge without cap applies full multiplier" $ do
        let config = (mkConfig PER_TRIP) {surgeCap = Nothing}
            input = mkInput {surgeMultiplier = Just 1.5}
            result = calculateCorporateFare config input
        result.surgeApplied @?= True
        result.surgeCapped @?= False,
      testCase "Corporate discount reduces final fare" $ do
        let config = (mkConfig PER_TRIP) {corporateDiscountPct = Just 10.0}
            result = calculateCorporateFare config mkInput
        result.corporateDiscount @?= HighPrecMoney (toRational (0.1 :: Double) * 500)
        result.finalFare @?= HighPrecMoney (500 - toRational (0.1 :: Double) * 500),
      testCase "Zero discount has no effect" $ do
        let config = (mkConfig PER_TRIP) {corporateDiscountPct = Just 0.0}
            result = calculateCorporateFare config mkInput
        result.finalFare @?= HighPrecMoney 500,
      testCase "FLAT_ROUTE sets policyOverride to True" $ do
        let config = (mkConfig FLAT_ROUTE) {flatRate = Just (HighPrecMoney 250)}
            result = calculateCorporateFare config mkInput
        result.policyOverride @?= True,
      testCase "PER_SEAT_KM with zero seats gives zero distance fare" $ do
        let config = (mkConfig PER_SEAT_KM) {ratePerSeatKm = Just (HighPrecMoney 10)}
            input = mkInput {seats = 0, distanceKm = 5.0}
            result = calculateCorporateFare config input
        result.distanceFare @?= HighPrecMoney 0,
      -- NEW: Zero fare input
      testCase "PER_TRIP with zero actual fare" $ do
        let config = mkConfig PER_TRIP
            input = mkInput {actualFare = HighPrecMoney 0}
            result = calculateCorporateFare config input
        result.finalFare @?= HighPrecMoney 0,
      -- NEW: Zero distance
      testCase "HYBRID with zero distance gives only base fare" $ do
        let config = (mkConfig HYBRID) {baseRate = Just (HighPrecMoney 100), perKmRate = Just (HighPrecMoney 20)}
            input = mkInput {distanceKm = 0.0}
            result = calculateCorporateFare config input
        result.baseFare @?= HighPrecMoney 100
        result.distanceFare @?= HighPrecMoney 0
        result.finalFare @?= HighPrecMoney 100,
      -- NEW: Very large fare (overflow-like boundary)
      testCase "PER_TRIP with very large fare value" $ do
        let config = mkConfig PER_TRIP
            input = mkInput {actualFare = HighPrecMoney 999999999999}
            result = calculateCorporateFare config input
        result.finalFare @?= HighPrecMoney 999999999999,
      -- NEW: Very small fractional fare (currency precision)
      testCase "PER_SEAT_KM with fractional distance preserves precision" $ do
        let config = (mkConfig PER_SEAT_KM) {ratePerSeatKm = Just (HighPrecMoney 7)}
            input = mkInput {seats = 1, distanceKm = 0.001}
            result = calculateCorporateFare config input
        -- 0.001 * 1 * 7 = 0.007
        result.distanceFare @?= HighPrecMoney (toRational (0.001 :: Double) * 7),
      -- NEW: 100% discount makes fare zero
      testCase "100% corporate discount makes final fare zero" $ do
        let config = (mkConfig PER_TRIP) {corporateDiscountPct = Just 100.0}
            result = calculateCorporateFare config mkInput
        result.finalFare @?= HighPrecMoney 0,
      -- NEW: Discount greater than 100% is clamped to zero (subtractMoney guard)
      testCase "Discount > 100% yields zero final fare (clamped)" $ do
        let config = (mkConfig PER_TRIP) {corporateDiscountPct = Just 150.0}
            result = calculateCorporateFare config mkInput
        -- 150% of 500 = 750 discount, subtractMoney clamps to max 0
        result.finalFare @?= HighPrecMoney 0,
      -- NEW: Surge exactly at cap
      testCase "Surge exactly at cap is not marked as capped" $ do
        let config = (mkConfig PER_TRIP) {surgeCap = Just 1.5}
            input = mkInput {surgeMultiplier = Just 1.5}
            result = calculateCorporateFare config input
        result.surgeApplied @?= True
        result.surgeCapped @?= False,
      -- NEW: Surge just barely above cap
      testCase "Surge slightly above cap is marked as capped" $ do
        let config = (mkConfig PER_TRIP) {surgeCap = Just 1.5}
            input = mkInput {surgeMultiplier = Just 1.501}
            result = calculateCorporateFare config input
        result.surgeApplied @?= True
        result.surgeCapped @?= True,
      -- NEW: FLAT_ROUTE with no flat rate configured uses zero
      testCase "FLAT_ROUTE with no flat rate defaults to zero" $ do
        let config = mkConfig FLAT_ROUTE
            result = calculateCorporateFare config mkInput
        result.finalFare @?= HighPrecMoney 0,
      -- NEW: HYBRID with no rates configured uses zero
      testCase "HYBRID with no rates configured defaults to zero" $ do
        let config = mkConfig HYBRID
            result = calculateCorporateFare config mkInput
        result.baseFare @?= HighPrecMoney 0
        result.distanceFare @?= HighPrecMoney 0
        result.finalFare @?= HighPrecMoney 0,
      -- NEW: PER_SEAT_KM with no rate configured uses zero
      testCase "PER_SEAT_KM with no rate configured defaults to zero" $ do
        let config = mkConfig PER_SEAT_KM
            input = mkInput {seats = 5, distanceKm = 100.0}
            result = calculateCorporateFare config input
        result.distanceFare @?= HighPrecMoney 0,
      -- NEW: Negative seats
      testCase "PER_SEAT_KM with negative seats gives negative distance fare" $ do
        let config = (mkConfig PER_SEAT_KM) {ratePerSeatKm = Just (HighPrecMoney 10)}
            input = mkInput {seats = -1, distanceKm = 5.0}
            result = calculateCorporateFare config input
        (result.distanceFare < HighPrecMoney 0) @?= True,
      -- NEW: Validate billing with empty allowed models
      testCase "Validate billing with empty allowed models rejects all" $ do
        let policy = CorporatePolicy Nothing Nothing Nothing [] True
            result = validateBillingEntry policy PER_TRIP (HighPrecMoney 100) 5.0 Nothing
        result @?= BillingModelNotAllowed PER_TRIP,
      -- NEW: Validate billing with all limits unset passes
      testCase "Validate billing with no limits set always passes" $ do
        let policy = CorporatePolicy Nothing Nothing Nothing [PER_TRIP] True
            result = validateBillingEntry policy PER_TRIP (HighPrecMoney 999999) 99999.0 (Just 10.0)
        result @?= BillingValid,
      -- NEW: Validate billing - fare exactly at limit
      testCase "Validate billing - fare exactly at limit passes" $ do
        let policy = CorporatePolicy (Just (HighPrecMoney 500)) Nothing Nothing [PER_TRIP] True
            result = validateBillingEntry policy PER_TRIP (HighPrecMoney 500) 5.0 Nothing
        result @?= BillingValid,
      -- NEW: Validate billing - distance exactly at limit
      testCase "Validate billing - distance exactly at limit passes" $ do
        let policy = CorporatePolicy Nothing (Just 10.0) Nothing [PER_TRIP] True
            result = validateBillingEntry policy PER_TRIP (HighPrecMoney 100) 10.0 Nothing
        result @?= BillingValid,
      -- NEW: Validate billing - surge allowed with no multiplier passes
      testCase "Validate billing - surge not allowed but no surge present passes" $ do
        let policy = CorporatePolicy Nothing Nothing Nothing [PER_TRIP] False
            result = validateBillingEntry policy PER_TRIP (HighPrecMoney 100) 5.0 Nothing
        result @?= BillingValid,
      -- NEW: Validate billing - model check takes priority over fare check
      testCase "Validate billing - model not allowed takes priority over fare limit" $ do
        let policy = CorporatePolicy (Just (HighPrecMoney 100)) Nothing Nothing [PER_TRIP] True
            result = validateBillingEntry policy PER_SEAT_KM (HighPrecMoney 999) 5.0 Nothing
        result @?= BillingModelNotAllowed PER_SEAT_KM,
      -- NEW: Discount + surge interaction
      testCase "Discount and surge applied together" $ do
        let config = (mkConfig PER_TRIP) {surgeCap = Nothing, corporateDiscountPct = Just 20.0}
            input = mkInput {surgeMultiplier = Just 2.0}
            result = calculateCorporateFare config input
        result.surgeApplied @?= True
        -- fare = 500 * 2.0 = 1000, discount = 20% of 1000 = 200, final = 800
        result.finalFare @?= HighPrecMoney (toRational (800.0 :: Double))
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
        length outliers @?= 1,
      testCase "No outlier when stops are close together" $ do
        let stops =
              [ mkStopWithGender "s1" "MALE" 1,
                mkStopWithGender "s2" "MALE" 2,
                mkStopWithGender "s3" "MALE" 3
              ]
            outliers = detectOutlierStops 1000.0 stops
        outliers @?= [],
      testCase "No outliers with fewer than 3 stops" $ do
        let stops =
              [ mkStopWithGender "s1" "MALE" 1,
                mkStopWithGender "s2" "MALE" 2
              ]
            outliers = detectOutlierStops 1.0 stops
        outliers @?= [],
      testCase "Validate empty stops returns no violations" $ do
        let constraints = GenderConstraints True True True
            violations = validateRouteGenderSafety constraints ([] :: [RouteStopWithGender])
        violations @?= [],
      testCase "All-female stops keeps order unchanged after reorder attempt" $ do
        let constraints = GenderConstraints True True True
            stops =
              [ mkStopWithGender "s1" "FEMALE" 1,
                mkStopWithGender "s2" "FEMALE" 2,
                mkStopWithGender "s3" "FEMALE" 3
              ]
            reordered = reorderForGenderSafety constraints stops
        length reordered @?= 3
        -- When no male stops exist, order should be preserved since no swap is possible
        (head reordered).gender @?= "FEMALE",
      testCase "Both first and last violations detected simultaneously" $ do
        let constraints = GenderConstraints True True True
            stops =
              [ mkStopWithGender "s1" "FEMALE" 1,
                mkStopWithGender "s2" "MALE" 2,
                mkStopWithGender "s3" "FEMALE" 3
              ]
            violations = validateRouteGenderSafety constraints stops
        length violations @?= 2,
      testCase "Reorder fixes female last drop" $ do
        let constraints = GenderConstraints True False True
            stops =
              [ mkStopWithGender "s1" "MALE" 1,
                mkStopWithGender "s2" "MALE" 2,
                mkStopWithGender "s3" "FEMALE" 3
              ]
            reordered = reorderForGenderSafety constraints stops
        case reverse reordered of
          (lst : _) -> lst.gender @?= "MALE"
          _ -> assertFailure "Reordered list should not be empty",
      -- NEW: Single female stop - both first and last
      testCase "Single female stop triggers both violations" $ do
        let constraints = GenderConstraints True True True
            stops = [mkStopWithGender "s1" "FEMALE" 1]
            violations = validateRouteGenderSafety constraints stops
        length violations @?= 2,
      -- NEW: Single male stop - no violations
      testCase "Single male stop has no violations" $ do
        let constraints = GenderConstraints True True True
            stops = [mkStopWithGender "s1" "MALE" 1]
            violations = validateRouteGenderSafety constraints stops
        violations @?= [],
      -- NEW: Reorder single female stop cannot fix (no males available)
      testCase "Reorder single female stop remains unchanged" $ do
        let constraints = GenderConstraints True True True
            stops = [mkStopWithGender "s1" "FEMALE" 1]
            reordered = reorderForGenderSafety constraints stops
        length reordered @?= 1
        (head reordered).gender @?= "FEMALE",
      -- NEW: Unknown/empty gender string
      testCase "Non-standard gender string is not treated as FEMALE" $ do
        let constraints = GenderConstraints True True True
            stops =
              [ mkStopWithGender "s1" "" 1,
                mkStopWithGender "s2" "OTHER" 2,
                mkStopWithGender "s3" "MALE" 3
              ]
            violations = validateRouteGenderSafety constraints stops
        violations @?= [],
      -- NEW: Reorder renumbers sequence
      testCase "Reorder renumbers sequence numbers starting from 1" $ do
        let constraints = GenderConstraints True True True
            stops =
              [ mkStopWithGender "s1" "FEMALE" 10,
                mkStopWithGender "s2" "MALE" 20,
                mkStopWithGender "s3" "FEMALE" 30
              ]
            reordered = reorderForGenderSafety constraints stops
            seqNums = map (.sequenceNumber) reordered
        seqNums @?= [1, 2, 3],
      -- NEW: Large route with outliers
      testCase "Detect multiple outlier stops" $ do
        let stops =
              [ mkStopWithGender "s1" "MALE" 1,
                (mkStopWithGender "s2" "MALE" 2) {location = LatLong 30.0 90.0},
                mkStopWithGender "s3" "MALE" 3,
                (mkStopWithGender "s4" "MALE" 4) {location = LatLong 25.0 85.0},
                mkStopWithGender "s5" "MALE" 5
              ]
            outliers = detectOutlierStops 500.0 stops
        length outliers >= 1 @?= True,
      -- NEW: Outlier detection with single stop returns empty
      testCase "Detect outlier with single stop returns empty" $ do
        let stops = [mkStopWithGender "s1" "MALE" 1]
            outliers = detectOutlierStops 0.0 stops
        outliers @?= [],
      -- NEW: Outlier detection with zero threshold
      testCase "Detect outlier with zero threshold flags interior stops with detour" $ do
        let stops =
              [ mkStopWithGender "s1" "MALE" 1,
                (mkStopWithGender "s2" "MALE" 2) {location = LatLong 12.98 77.60},
                mkStopWithGender "s3" "MALE" 3
              ]
            outliers = detectOutlierStops 0.0 stops
        -- With threshold 0, any interior stop adding any detour is flagged
        length outliers >= 0 @?= True,
      -- NEW: fromRouteStop conversion preserves all fields
      testCase "fromRouteStop preserves all fields" $ do
        let rs = RouteStop "test-id" (LatLong 12.97 77.59) 42 "emp-99" "FEMALE" DROP
            converted = fromRouteStop rs
        converted.stopId @?= "test-id"
        converted.location @?= LatLong 12.97 77.59
        converted.sequenceNumber @?= 42
        converted.gender @?= "FEMALE"
        converted.direction @?= DROP,
      -- NEW: Constraints disabled (all False)
      testCase "All constraints disabled returns no violations" $ do
        let constraints = GenderConstraints True False False
            stops =
              [ mkStopWithGender "s1" "FEMALE" 1,
                mkStopWithGender "s2" "MALE" 2,
                mkStopWithGender "s3" "FEMALE" 3
              ]
            violations = validateRouteGenderSafety constraints stops
        violations @?= [],
      -- NEW: Alternating male-female pattern
      testCase "Alternating MALE-FEMALE pattern has no violations" $ do
        let constraints = GenderConstraints True True True
            stops =
              [ mkStopWithGender "s1" "MALE" 1,
                mkStopWithGender "s2" "FEMALE" 2,
                mkStopWithGender "s3" "MALE" 3,
                mkStopWithGender "s4" "FEMALE" 4,
                mkStopWithGender "s5" "MALE" 5
              ]
            violations = validateRouteGenderSafety constraints stops
        -- First is MALE (OK), last is MALE (OK)
        violations @?= []
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
