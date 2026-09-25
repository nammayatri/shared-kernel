{-# LANGUAGE DerivingStrategies #-}
{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Payout.Types where

import Data.Aeson.Types
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto (derivePersistField)

data PayoutService = AAJuspay | Juspay | Stripe | StripeTest | HdfcCbx
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 'BulkFlow' partners have no single-order API: payouts are batched and submitted
-- together, then resolved by polling. See Kernel.External.Payout.Interface.HdfcCbx.
data PayoutServiceFlow = StripeFlow | JuspayFlow | BulkFlow
  deriving stock (Show, Generic, Eq)

castPayoutServiceFlow :: PayoutService -> PayoutServiceFlow
castPayoutServiceFlow Stripe = StripeFlow
castPayoutServiceFlow StripeTest = StripeFlow
castPayoutServiceFlow Juspay = JuspayFlow
castPayoutServiceFlow AAJuspay = JuspayFlow
castPayoutServiceFlow HdfcCbx = BulkFlow

$(mkBeamInstancesForEnumAndList ''PayoutService)
derivePersistField "PayoutService"

-- | How often a submitted batch is asked about, and for how long.
--
-- Every number here is a partner's operational rule rather than ours, which is why it is
-- configuration and not code. Stored per merchant operating city on the payout service config,
-- so two cities on different partner setups can differ.
--
-- Every time is an offset from the moment the batch was submitted (call it T), so the plan needs
-- no time zone and no calendar arithmetic. The shape is: a burst of checks on the first day, then
-- one check at the end of each of the next @tailChecks@ days (T+48h, T+72h, ...).
--
-- One check is up to @maxCallsPerCheck@ calls, because a partner may answer the first call with
-- "ask again shortly" and the real status only on the next one.
data BulkStatusCheckPlan = BulkStatusCheckPlan
  { -- | Delay from submission to the first check. Partners need time to ingest a batch.
    firstCheckDelayMinutes :: Int,
    -- | Checks on the first day.
    burstChecks :: Int,
    -- | Gap between two checks of the burst.
    burstGapMinutes :: Int,
    -- | Checks after the first day: one at the end of each following day.
    tailChecks :: Int,
    -- | How far apart those later checks are, and how far the first of them is from the end of
    -- the first day. A day in production; minutes in a test, which is the only way a three-day
    -- plan can be exercised locally.
    tailGapMinutes :: Int,
    -- | Calls one check may make. The first primes the partner, the second reads the answer.
    maxCallsPerCheck :: Int,
    -- | Gap between the priming call and the one that reads the answer.
    readCallDelaySeconds :: Int,
    -- | Gap before retrying a call that failed outright.
    errorRetryDelaySeconds :: Int,
    -- | The partner's ceiling per batch per day. Enforced on the plan by
    -- 'sanitizeBulkStatusCheckPlan', so nothing has to count calls at runtime.
    maxCallsPerDay :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | What a partner without its own published cadence gets, and what a config row without a
-- plan falls back to. These are the HDFC CBX numbers agreed with their team: first check at
-- T+2h, then every 2 h, six times; one check at T+48h and one at T+72h; two calls a check,
-- 15 s apart; twelve calls a day.
defaultBulkStatusCheckPlan :: BulkStatusCheckPlan
defaultBulkStatusCheckPlan =
  BulkStatusCheckPlan
    { firstCheckDelayMinutes = 120,
      burstChecks = 6,
      burstGapMinutes = 120,
      tailChecks = 2,
      tailGapMinutes = 24 * 60,
      maxCallsPerCheck = 2,
      readCallDelaySeconds = 15,
      errorRetryDelaySeconds = 60,
      maxCallsPerDay = 12
    }

-- | Force a plan into a range the scheduler can actually use.
--
-- The plan arrives from a hand-edited JSON column, so it is untrusted input. Values are clamped
-- rather than rejected, because refusing to check a batch that is already holding money is worse
-- than checking it on slightly wrong numbers. Two clamps matter beyond the obvious ones:
-- @burstChecks@ can never ask for more calls in a day than the partner allows, and the burst
-- always fits inside the first day.
sanitizeBulkStatusCheckPlan :: BulkStatusCheckPlan -> BulkStatusCheckPlan
sanitizeBulkStatusCheckPlan p =
  BulkStatusCheckPlan
    { firstCheckDelayMinutes = firstDelay,
      burstChecks = min burstAllowed (atLeast 1 p.burstChecks),
      burstGapMinutes = gap,
      tailChecks = atLeast 0 p.tailChecks,
      tailGapMinutes = atLeast 1 p.tailGapMinutes,
      maxCallsPerCheck = callsPerCheck,
      readCallDelaySeconds = atLeast 5 p.readCallDelaySeconds,
      errorRetryDelaySeconds = atLeast 5 p.errorRetryDelaySeconds,
      maxCallsPerDay = callsPerDay
    }
  where
    atLeast n = max n
    firstDelay = atLeast 1 p.firstCheckDelayMinutes
    gap = atLeast 1 p.burstGapMinutes
    callsPerCheck = min 3 (atLeast 1 p.maxCallsPerCheck)
    callsPerDay = atLeast 1 p.maxCallsPerDay
    -- Never plan more calls in the first day than the partner allows, and never let the burst
    -- run past the end of that day.
    burstAllowed = max 1 (min (callsPerDay `div` callsPerCheck) (((1440 - firstDelay) `div` gap) + 1))
