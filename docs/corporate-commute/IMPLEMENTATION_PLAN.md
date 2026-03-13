# Namma Commute: Shared Kernel Implementation Plan (shared-kernel repo)

## Overview

This document details changes needed in the shared-kernel (mobility-core) library for the Corporate Commute platform. The shared-kernel provides database abstractions, external service integrations, core types, and utilities.

**Key principle**: Extend existing provider interfaces; add corporate-specific utilities to shared infrastructure.

---

## Development Process: 3-Loop Model

Every module follows a Design/Code/Test loop with maker/checker separation:

1. **Design Loop** — Maker writes the type signatures, data types, and module interface. Checker reviews for consistency with existing kernel patterns (`FlowR` monad, provider pattern, error hierarchy). No implementation code until design is approved.
2. **Code Loop** — Maker implements the module. Checker reviews for correctness, edge cases, and adherence to the design. Focus on: no partial functions, proper error types (not `error`/`undefined`), and `HighPrecMoney` for all monetary values.
3. **Test Loop** — Maker writes Tasty/HUnit tests (see Section 5). Checker verifies coverage of edge cases and constraint combinations. Tests must pass before the module merges.

Maker and checker must be different people for each loop. A module is not "done" until all three loops close.

---

## 1. New Modules

### 1.1 `src/Kernel/Utils/StateMachine.hs`

Generic type-class based state machine validator. Prevents invalid status transitions at the type level where possible, validated at runtime otherwise.

```haskell
module Kernel.Utils.StateMachine where

-- Core type class: define valid transitions for any status enum
class (Eq s, Show s) => StateMachine s where
  validTransitions :: s -> [s]
  isTerminal       :: s -> Bool
  initialState     :: s

-- Transition result
data TransitionResult s
  = TransitionOk s
  | InvalidTransition s s [s]  -- from, attempted, allowed
  deriving (Eq, Show)

-- Attempt a state transition; returns error context on failure
transition :: StateMachine s => s -> s -> TransitionResult s

-- Bulk: validate a full sequence of states
validateSequence :: StateMachine s => [s] -> [TransitionResult s]
```

Instances for all corporate entity statuses:

```haskell
-- Corporate entity lifecycle
instance StateMachine CorporateEntityStatus where
  -- PENDING_VERIFICATION -> ACTIVE -> SUSPENDED | INACTIVE
  -- SUSPENDED -> ACTIVE | INACTIVE

-- Corporate trip lifecycle
instance StateMachine CorporateTripStatus where
  -- SCHEDULED -> DRIVER_ASSIGNED -> IN_PROGRESS -> COMPLETED
  --                              -> CANCELLED
  -- SCHEDULED -> CANCELLED

-- Corporate employee lifecycle
instance StateMachine EmployeeStatus where
  -- INVITED -> ACTIVE -> DEACTIVATED
  -- ACTIVE -> SUSPENDED -> ACTIVE | DEACTIVATED
```

### 1.2 `src/Kernel/Types/CorporateTypes.hs`

Core corporate types shared across rider-app and driver-app:

```haskell
module Kernel.Types.CorporateTypes where

-- Tag-based booking context (replaces TripCategory extension approach)
data CorporateBookingContext = CorporateBookingContext
  { corporateEntityId :: Id CorporateEntity
  , shiftId           :: Maybe (Id CorporateShift)
  , routeId           :: Maybe (Id CorporateRoute)
  , billingModel      :: CorporateBillingModel
  , policyId          :: Id CorporatePolicy
  , employeeId        :: Id CorporateEmployee
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- Attach to any Booking via a tag lookup, no schema changes to Booking table.
-- rider-app checks for CorporateBookingContext tag at booking creation;
-- if present, corporate billing/routing/safety logic activates.

-- Billing models
data CorporateBillingModel
  = PER_TRIP
  | PER_EMPLOYEE_MONTH
  | PER_SEAT_KM
  | FLAT_ROUTE
  | HYBRID
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

-- Billing calculation result
data CorporateBillingResult = CorporateBillingResult
  { baseFare :: HighPrecMoney
  , distanceFare :: Maybe HighPrecMoney
  , corporateDiscount :: Maybe HighPrecMoney
  , surgeApplied :: Maybe Double
  , surgeCapped :: Bool
  , policyOverride :: Bool
  , finalFare :: HighPrecMoney
  , billToWallet :: Bool
  }

-- Route optimization types
data RouteOptimizationRequest = RouteOptimizationRequest
  { employeeLocations :: [(Double, Double, Text)]  -- lat, lon, employeeId
  , officeLocation :: (Double, Double)
  , direction :: RouteDirection
  , maxDetourMinutes :: Int
  , maxCapacity :: Int
  , genderConstraints :: Maybe GenderConstraints
  , timeWindow :: (TimeOfDay, TimeOfDay)
  }

data GenderConstraints = GenderConstraints
  { isNightShift :: Bool
  , femaleEmployeeIds :: [Text]
  , noFemaleFirstPickup :: Bool
  , noFemaleLastDrop :: Bool
  }

data OptimizedRoute = OptimizedRoute
  { stops :: [RouteStop]
  , totalDistanceMeters :: Int
  , totalDurationMinutes :: Int
  , polyline :: Text
  , vehiclesNeeded :: Int
  }

data RouteStop = RouteStop
  { location :: (Double, Double)
  , address :: Text
  , sequence :: Int
  , employeeIds :: [Text]
  , estimatedArrivalOffset :: Int  -- minutes from start
  }
```

### 1.3 `src/Kernel/Utils/CorporateRouting.hs`

Route optimization utilities. **MVP uses Google Maps waypoint optimization** (Directions API with `optimize:true`, 25 waypoints max) via the existing `Kernel/External/Maps/Interface`. No custom TSP solver in Phase 1.

```haskell
module Kernel.Utils.CorporateRouting where

-- Stop clustering using K-means on geolocation.
-- Clusters are sized to respect the 25-waypoint API limit and vehicle capacity.
clusterStops :: [(Double, Double, Text)] -> Int -> [[(Double, Double, Text)]]

-- Waypoint optimization via Google Maps Directions API (optimize:true).
-- Delegates to Kernel/External/Maps/Interface/getRoutes.
-- Returns optimized stop order within API's 25-waypoint limit.
optimizeStopSequence :: (Double, Double) -> [(Double, Double)] -> MapsServiceConfig -> FlowR r [Int]

-- Apply gender safety constraints to route (post-optimization reordering)
applyGenderConstraints :: GenderConstraints -> [RouteStop] -> [RouteStop]

-- Split employees into multiple routes based on capacity
splitIntoRoutes :: Int -> [RouteStop] -> [[RouteStop]]

-- Full route optimization pipeline
optimizeRoute :: RouteOptimizationRequest -> MapsServiceConfig -> FlowR r OptimizedRoute
```

Reuses:
- `Kernel/External/Maps/Interface/` for distance matrix and waypoint optimization
- `Kernel/External/Maps/Types.hs` for location types

**Phase 2 (scale beyond API limits)**: Custom TSP solver (nearest-neighbor heuristic + 2-opt improvement) for routes exceeding 25 waypoints. Will be added when corporate clients require routes with 25+ stops per vehicle.

### 1.4 `src/Kernel/Utils/CorporateBilling.hs`

Billing calculation utilities.

**Critical**: All wallet operations (debit, top-up, refund) MUST use atomic decrements. Use Juspay wallet API's native atomic debit endpoint, or if hitting the DB directly, use `SELECT FOR UPDATE` with a single atomic decrement query. Never read-modify-write (read balance, compute new balance, write) -- this causes race conditions under concurrent trip completions.

```haskell
module Kernel.Utils.CorporateBilling where

-- Calculate corporate trip cost based on billing model
calculateCorporateFare
  :: CorporateBillingModel
  -> Maybe HighPrecMoney       -- negotiated rate
  -> HighPrecMoney             -- actual fare
  -> Maybe Double              -- surge
  -> Maybe Double              -- surge cap
  -> CorporateBillingResult

-- Atomic wallet debit for a completed trip.
-- Uses Juspay wallet API atomic decrement (or SELECT FOR UPDATE fallback).
-- Returns InsufficientBalance error if wallet balance < amount.
debitCorporateWallet
  :: Id CorporateWallet
  -> HighPrecMoney
  -> FlowR r (Either WalletError WalletTransaction)

-- Generate trip sheet for a billing period
generateTripSheet
  :: Id CorporateEntity
  -> UTCTime                   -- period start
  -> UTCTime                   -- period end
  -> FlowR r [TripSheetEntry]

-- Maker-checker billing validation
validateBillingEntry
  :: TripSheetEntry
  -> CorporatePolicy
  -> BillingValidationResult
```

Reuses:
- `Kernel/Types/Common.hs` -- HighPrecMoney, Currency
- `Kernel/External/Payment/` -- Juspay wallet operations (atomic debit endpoint)

### 1.5 `src/Kernel/Utils/GenderSafetyRules.hs`

Women's safety rules for routing and allocation:

```haskell
module Kernel.Utils.GenderSafetyRules where

-- Check if a route violates gender safety rules
validateRouteGenderSafety :: Bool -> [RouteStopWithGender] -> [GenderSafetyViolation]

-- Reorder route to comply with safety rules
reorderForGenderSafety :: [RouteStopWithGender] -> [RouteStopWithGender]

-- Check if a driver assignment is safe for night shift
validateDriverForNightShift :: DriverProfile -> NightShiftSafetyConfig -> Bool

-- Outlier stop detection: flag stops that add more than maxDetourMinutes
-- to the route. These stops are pulled out for manual review rather than
-- silently degrading route quality for all other passengers.
data OutlierStopResult = OutlierStopResult
  { feasibleStops :: [RouteStop]
  , outlierStops  :: [OutlierStop]  -- stops exceeding detour threshold
  }

data OutlierStop = OutlierStop
  { stop           :: RouteStop
  , addedMinutes   :: Int           -- detour this stop adds
  , maxAllowed     :: Int           -- maxDetourMinutes from request
  }

detectOutlierStops
  :: Int                          -- maxDetourMinutes
  -> (Double, Double)             -- office/anchor location
  -> [RouteStop]
  -> MapsServiceConfig
  -> FlowR r OutlierStopResult
```

---

## 2. Modified Modules

### 2.1 `src/Kernel/External/Payment/Juspay/`

Extend for corporate wallet operations:

```
Changes:
- Add corporate wallet creation (linked to CorporateEntity)
- Add wallet top-up (bank transfer, credit card)
- Add per-trip atomic wallet debit (MUST use atomic decrement, not read-modify-write)
- Add batch settlement (end of billing cycle)
- Add wallet balance query
- Add refund to corporate wallet
```

### 2.2 `src/Kernel/External/SMS/`

Add corporate SMS templates:

```
New templates:
- CORPORATE_SHIFT_REMINDER: "Your pickup for [shift] is at [time] from [location]"
- CORPORATE_VEHICLE_APPROACHING: "Your vehicle [number] is [X] min away"
- CORPORATE_SAFE_DROP_CONFIRM: "Please confirm safe arrival. Reply YES or call [number]"
- CORPORATE_ROSTER_UPDATE: "Your shift has been updated. New pickup: [time]"
- CORPORATE_POLICY_ALERT: "Ride request exceeds corporate policy. Budget: [amount]"
```

### 2.3 `src/Kernel/External/Notification/`

Add corporate push notification types:

```
New notification types:
- CORPORATE_RIDE_ASSIGNED
- CORPORATE_RIDE_REMINDER (30 min, 15 min, 5 min before)
- CORPORATE_VEHICLE_APPROACHING
- CORPORATE_PICKUP_ARRIVED
- CORPORATE_RIDE_STARTED
- CORPORATE_RIDE_COMPLETED
- CORPORATE_SAFE_DROP_REQUIRED
- CORPORATE_SHIFT_CHANGE
- CORPORATE_ROUTE_CHANGE
```

### 2.4 `src/Kernel/External/Maps/`

Extend for bulk operations:

```
Changes:
- Add bulk distance matrix API (multiple origins to single destination)
- Add stop clustering utilities (for route optimization)
- Reuse existing getDistance, getRoutes interfaces
- Expose waypoint optimization parameter for Google Maps Directions API
```

### 2.5 `src/Kernel/Streaming/`

Add Kafka topics for corporate events:

```
New topics:
- corporate-trip-event (trip lifecycle events)
- corporate-billing-event (billing/wallet events)
- corporate-safety-event (safety incidents)
- corporate-compliance-event (document expiry, compliance alerts)
```

---

## 3. Implementation Sequence

**Important**: shared-kernel changes must land BEFORE corresponding nammayatri backend sprints, as rider-app/driver-app depend on these utilities.

### Pre-Sprint Buffer (Weeks 1-2): Design and Setup

2-week buffer before backend Sprint 1 starts. Used for:
- [ ] Finalize all type signatures and module interfaces (Design Loop for all modules)
- [ ] Set up test scaffolding (Tasty test modules, CI integration)
- [ ] Review CorporateBookingContext tag approach with rider-app team
- [ ] Confirm Google Maps waypoint optimization API access and rate limits

### Sprint 1 (Weeks 3-4): Core Types, State Machine & Routing (land before backend Sprint 3)
- [ ] Create `Kernel/Utils/StateMachine.hs` with all corporate status instances
- [ ] Create `Kernel/Types/CorporateTypes.hs` (including `CorporateBookingContext`)
- [ ] Create `Kernel/Utils/CorporateRouting.hs` (Google Maps waypoint optimization, clustering, gender constraints)
- [ ] Create `Kernel/Utils/GenderSafetyRules.hs` (including outlier stop detection)
- [ ] Extend Maps for bulk distance matrix and waypoint optimization parameter
- [ ] Write and pass all Sprint 1 tests (see Section 5)

### Sprint 2-3 (Weeks 5-8): Billing & Wallet (land before backend Sprint 7)
- [ ] Create `Kernel/Utils/CorporateBilling.hs` (atomic wallet operations)
- [ ] Extend Payment/Juspay for corporate wallet operations
- [ ] Write and pass all Sprint 2-3 tests (see Section 5)

### Sprint 4-5 (Weeks 9-12): Notifications & Events
- [ ] Add corporate SMS templates
- [ ] Add corporate push notification types
- [ ] Add Kafka topics for corporate events
- [ ] Integration testing with nammayatri backend

---

## 4. Dependencies

```
shared-kernel (this repo)
  └── consumed by: rider-app, driver-app, dashboard
  └── external: Google Maps API (waypoint optimization, distance matrix)
  └── external: Juspay wallet API (atomic debit)
```

---

## 5. Testing Requirements

Every utility function gets Tasty/HUnit tests written alongside the code (Test Loop). Tests live in `lib/mobility-core/test/`. No module merges without passing tests.

### 5.1 StateMachine Tests (`StateMachineTests.hs`)

- Valid transition succeeds for every edge in each instance's transition graph
- Invalid transition returns `InvalidTransition` with correct allowed-states list
- Terminal states reject all transitions
- `validateSequence` catches first invalid transition in a chain

### 5.2 Routing Tests (`CorporateRoutingTests.hs`)

Constraint satisfaction focus:

- **Cluster size**: `clusterStops` never produces a cluster with more than 25 stops (API limit)
- **Capacity**: `splitIntoRoutes` respects maxCapacity; no route has more employees than seats
- **Round-trip consistency**: optimizing a route twice with the same input produces the same output
- **Single stop**: route with 1 employee returns trivial route (office -> stop -> office)
- **Gender constraint satisfaction**: after `applyGenderConstraints`, no female employee is first pickup or last drop when constraints require it
- **Outlier detection**: a stop adding 2x `maxDetourMinutes` is flagged; a stop within threshold is not

### 5.3 Billing Tests (`CorporateBillingTests.hs`)

Per-model accuracy:

- **PER_TRIP**: fare equals negotiated rate when present, actual fare otherwise
- **PER_SEAT_KM**: fare scales linearly with seats * distance
- **FLAT_ROUTE**: fare is constant regardless of actual distance
- **HYBRID**: base component + per-km component calculated correctly
- **Surge capping**: when surge exceeds cap, `surgeCapped = True` and fare uses capped surge
- **Zero fare**: billing model handles zero-distance trips without division errors
- **Wallet debit atomicity**: concurrent debit simulation (if testable) -- at minimum, verify `debitCorporateWallet` returns `InsufficientBalance` when amount > balance

### 5.4 Gender Safety Rules Tests (`GenderSafetyRulesTests.hs`)

All constraint combinations:

- **Night shift, females present, constraints on**: first pickup is not female, last drop is not female
- **Night shift, no females**: no reordering needed, route unchanged
- **Day shift, constraints on**: constraints still applied (configurable per-policy)
- **Day shift, constraints off**: route unchanged regardless of gender mix
- **All-female route**: `reorderForGenderSafety` returns a valid ordering (not empty/error)
- **Single-female among males**: female is neither first nor last
- **Outlier + gender interaction**: outlier detection runs before gender reordering; flagged stops are excluded from gender constraint evaluation
