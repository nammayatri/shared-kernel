{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.CorporateBilling where

import Kernel.Prelude
import Kernel.Types.CorporateTypes
import Kernel.Types.Price (HighPrecMoney (..))

-- | Configuration for corporate fare calculation
data CorporateFareConfig = CorporateFareConfig
  { billingModel :: CorporateBillingModel,
    negotiatedRate :: Maybe HighPrecMoney,
    ratePerSeatKm :: Maybe HighPrecMoney,
    flatRate :: Maybe HighPrecMoney,
    baseRate :: Maybe HighPrecMoney,
    perKmRate :: Maybe HighPrecMoney,
    surgeCap :: Maybe Double,
    corporateDiscountPct :: Maybe Double
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Input for fare calculation
data CorporateFareInput = CorporateFareInput
  { actualFare :: HighPrecMoney,
    distanceKm :: Double,
    seats :: Int,
    surgeMultiplier :: Maybe Double
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Policy limits for billing validation
data CorporatePolicy = CorporatePolicy
  { maxFarePerTrip :: Maybe HighPrecMoney,
    maxDistanceKm :: Maybe Double,
    maxTripsPerDay :: Maybe Int,
    allowedBillingModels :: [CorporateBillingModel],
    allowSurge :: Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data BillingValidationResult
  = BillingValid
  | FareExceedsLimit HighPrecMoney HighPrecMoney
  | DistanceExceedsLimit Double Double
  | BillingModelNotAllowed CorporateBillingModel
  | SurgeNotAllowed
  deriving (Eq, Show)

-- | Calculate corporate fare based on billing model.
calculateCorporateFare :: CorporateFareConfig -> CorporateFareInput -> CorporateBillingResult
calculateCorporateFare config input =
  let (base, dist, surgeApplied', surgeCapped', policyOvr) = case config.billingModel of
        PER_TRIP -> calculatePerTrip config input
        PER_SEAT_KM -> calculatePerSeatKm config input
        FLAT_ROUTE -> calculateFlatRoute config
        HYBRID -> calculateHybrid config input
        PER_EMPLOYEE_MONTH -> calculatePerEmployeeMonth input
      discountPct = fromMaybe 0 config.corporateDiscountPct
      totalBeforeDiscount = addMoney base dist
      discount = scaleMoney (discountPct / 100.0) totalBeforeDiscount
      final = subtractMoney totalBeforeDiscount discount
   in CorporateBillingResult
        { baseFare = base,
          distanceFare = dist,
          corporateDiscount = discount,
          surgeApplied = surgeApplied',
          surgeCapped = surgeCapped',
          policyOverride = policyOvr,
          finalFare = final,
          billToWallet = False
        }

-- | Validate a trip against corporate policy limits.
validateBillingEntry :: CorporatePolicy -> CorporateBillingModel -> HighPrecMoney -> Double -> Maybe Double -> BillingValidationResult
validateBillingEntry policy model fare distKm surgeMult
  | model `notElem` policy.allowedBillingModels = BillingModelNotAllowed model
  | not policy.allowSurge && isJust surgeMult = SurgeNotAllowed
  | Just maxFare <- policy.maxFarePerTrip, fare > maxFare = FareExceedsLimit fare maxFare
  | Just maxDist <- policy.maxDistanceKm, distKm > maxDist = DistanceExceedsLimit distKm maxDist
  | otherwise = BillingValid

-- Internal helpers

calculatePerTrip :: CorporateFareConfig -> CorporateFareInput -> (HighPrecMoney, HighPrecMoney, Bool, Bool, Bool)
calculatePerTrip config input =
  let baseFare' = fromMaybe input.actualFare config.negotiatedRate
      policyOvr = isJust config.negotiatedRate
      (applied, capped, fare) = applySurge config.surgeCap input.surgeMultiplier baseFare'
   in (fare, HighPrecMoney 0, applied, capped, policyOvr)

calculatePerSeatKm :: CorporateFareConfig -> CorporateFareInput -> (HighPrecMoney, HighPrecMoney, Bool, Bool, Bool)
calculatePerSeatKm config input =
  let rate = fromMaybe (HighPrecMoney 0) config.ratePerSeatKm
      dist = scaleMoney (input.distanceKm * fromIntegral input.seats) rate
   in (HighPrecMoney 0, dist, False, False, False)

calculateFlatRoute :: CorporateFareConfig -> (HighPrecMoney, HighPrecMoney, Bool, Bool, Bool)
calculateFlatRoute config =
  let flat = fromMaybe (HighPrecMoney 0) config.flatRate
   in (flat, HighPrecMoney 0, False, False, True)

calculateHybrid :: CorporateFareConfig -> CorporateFareInput -> (HighPrecMoney, HighPrecMoney, Bool, Bool, Bool)
calculateHybrid config input =
  let base = fromMaybe (HighPrecMoney 0) config.baseRate
      perKm = fromMaybe (HighPrecMoney 0) config.perKmRate
      dist = scaleMoney input.distanceKm perKm
   in (base, dist, False, False, False)

calculatePerEmployeeMonth :: CorporateFareInput -> (HighPrecMoney, HighPrecMoney, Bool, Bool, Bool)
calculatePerEmployeeMonth input =
  (input.actualFare, HighPrecMoney 0, False, False, False)

applySurge :: Maybe Double -> Maybe Double -> HighPrecMoney -> (Bool, Bool, HighPrecMoney)
applySurge _ Nothing fare = (False, False, fare)
applySurge surgeCap (Just mult) fare
  | mult <= 1.0 = (False, False, fare)
  | otherwise =
      let effectiveMult = case surgeCap of
            Just cap -> min mult cap
            Nothing -> mult
          capped = maybe False (mult >) surgeCap
       in (True, capped, scaleMoney effectiveMult fare)

-- | Money arithmetic helpers operating on HighPrecMoney
addMoney :: HighPrecMoney -> HighPrecMoney -> HighPrecMoney
addMoney (HighPrecMoney a) (HighPrecMoney b) = HighPrecMoney (a + b)

-- | Subtract money, clamping to zero to prevent negative balances
subtractMoney :: HighPrecMoney -> HighPrecMoney -> HighPrecMoney
subtractMoney (HighPrecMoney a) (HighPrecMoney b) = HighPrecMoney (max 0 (a - b))

scaleMoney :: Double -> HighPrecMoney -> HighPrecMoney
scaleMoney factor (HighPrecMoney a) = HighPrecMoney (a * toRational factor)

-- | Validate a wallet top-up amount and currency
validateTopUpAmount :: HighPrecMoney -> Text -> CorporateWalletInfo -> Either Text ()
validateTopUpAmount (HighPrecMoney amt) currency walletInfo
  | amt <= 0 = Left "Top-up amount must be positive"
  | currency /= walletInfo.walletCurrency = Left "Currency mismatch: top-up currency does not match wallet currency"
  | not walletInfo.walletActive = Left "Wallet is not active"
  | otherwise = Right ()

-- | Minimal wallet info needed for top-up validation (avoids depending on full domain types)
data CorporateWalletInfo = CorporateWalletInfo
  { walletCurrency :: Text,
    walletActive :: Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
