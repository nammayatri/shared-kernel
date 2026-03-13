# Namma Commute: Shared Kernel Implementation Plan (shared-kernel repo)

## Overview

This document details changes needed in the shared-kernel (mobility-core) library for the Corporate Commute platform. The shared-kernel provides database abstractions, external service integrations, core types, and utilities.

**Key principle**: Extend existing provider interfaces; add corporate-specific utilities to shared infrastructure.

---

## 1. New Modules

### 1.1 `src/Kernel/Types/CorporateTypes.hs`

Core corporate types shared across rider-app and driver-app:

```haskell
module Kernel.Types.CorporateTypes where

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

### 1.2 `src/Kernel/Utils/CorporateRouting.hs`

Route optimization utilities:

```haskell
module Kernel.Utils.CorporateRouting where

-- Stop clustering using K-means on geolocation
clusterStops :: [(Double, Double, Text)] -> Int -> [[(Double, Double, Text)]]

-- Route sequence optimization (nearest-neighbor heuristic + 2-opt improvement)
optimizeStopSequence :: (Double, Double) -> [(Double, Double)] -> IO [Int]

-- Apply gender safety constraints to route
applyGenderConstraints :: GenderConstraints -> [RouteStop] -> [RouteStop]

-- Split employees into multiple routes based on capacity
splitIntoRoutes :: Int -> [RouteStop] -> [[RouteStop]]

-- Full route optimization pipeline
optimizeRoute :: RouteOptimizationRequest -> MapsServiceConfig -> FlowR r OptimizedRoute
```

Reuses:
- `Kernel/External/Maps/Interface/` for distance matrix
- `Kernel/External/Maps/Types.hs` for location types

### 1.3 `src/Kernel/Utils/CorporateBilling.hs`

Billing calculation utilities:

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
- `Kernel/External/Payment/` -- Juspay wallet operations

### 1.4 `src/Kernel/Utils/GenderSafetyRules.hs`

Women's safety rules for routing and allocation:

```haskell
module Kernel.Utils.GenderSafetyRules where

-- Check if a route violates gender safety rules
validateRouteGenderSafety :: Bool -> [RouteStopWithGender] -> [GenderSafetyViolation]

-- Reorder route to comply with safety rules
reorderForGenderSafety :: [RouteStopWithGender] -> [RouteStopWithGender]

-- Check if a driver assignment is safe for night shift
validateDriverForNightShift :: DriverProfile -> NightShiftSafetyConfig -> Bool
```

---

## 2. Modified Modules

### 2.1 `src/Kernel/External/Payment/Juspay/`

Extend for corporate wallet operations:

```
Changes:
- Add corporate wallet creation (linked to CorporateEntity)
- Add wallet top-up (bank transfer, credit card)
- Add per-trip wallet debit
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

### Sprint 1 (Weeks 1-2): Core Types & Routing (land before backend Sprint 3)
- [ ] Create `Kernel/Types/CorporateTypes.hs`
- [ ] Create `Kernel/Utils/CorporateRouting.hs` (TSP solver, stop clustering)
- [ ] Create `Kernel/Utils/GenderSafetyRules.hs`
- [ ] Extend Maps for bulk distance matrix
- [ ] Build and test

### Sprint 2-3 (Weeks 3-6): Billing & Wallet (land before backend Sprint 7)
- [ ] Create `Kernel/Utils/CorporateBilling.hs`
- [ ] Extend Payment/Juspay for corporate wallet operations
- [ ] Build and test

### Sprint 4-5 (Weeks 7-10): Notifications & Events
- [ ] Add corporate SMS templates
- [ ] Add corporate push notification types
- [ ] Add Kafka topics for corporate events
- [ ] Integration testing with nammayatri backend
