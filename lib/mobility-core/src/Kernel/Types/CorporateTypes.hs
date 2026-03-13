{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.CorporateTypes where

import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.Price (HighPrecMoney (..))

data CorporateBillingModel
  = PER_TRIP
  | PER_EMPLOYEE_MONTH
  | PER_SEAT_KM
  | FLAT_ROUTE
  | HYBRID
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema, Read, Ord, Enum, Bounded)

data CorporateBookingContext = CorporateBookingContext
  { entityId :: Text,
    employeeId :: Text,
    shiftId :: Text,
    routeId :: Text,
    policyId :: Text,
    billingMode :: CorporateBillingModel
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data CorporateBillingResult = CorporateBillingResult
  { baseFare :: HighPrecMoney,
    distanceFare :: HighPrecMoney,
    corporateDiscount :: HighPrecMoney,
    surgeApplied :: Bool,
    surgeCapped :: Bool,
    policyOverride :: Bool,
    finalFare :: HighPrecMoney,
    billToWallet :: Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data RouteDirection
  = PICKUP
  | DROP
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema, Read, Ord, Enum, Bounded)

data RouteStop = RouteStop
  { stopId :: Text,
    location :: LatLong,
    sequenceNumber :: Int,
    employeeId :: Text,
    gender :: Text,
    direction :: RouteDirection
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data GenderConstraints = GenderConstraints
  { isNightShift :: Bool,
    noFemaleFirstPickup :: Bool,
    noFemaleLastDrop :: Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data RouteOptimizationRequest = RouteOptimizationRequest
  { stops :: [RouteStop],
    vehicleCapacity :: Int,
    direction :: RouteDirection,
    genderConstraints :: GenderConstraints,
    depotLocation :: LatLong
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data OptimizedRoute = OptimizedRoute
  { routeStops :: [RouteStop],
    totalDistanceMeters :: Double,
    vehicleCount :: Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
