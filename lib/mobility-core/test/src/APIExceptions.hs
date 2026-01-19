{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module APIExceptions (httpExceptionTests) where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error.BaseError.HTTPError
import Test.Tasty
import TestSilentIOLogger ()

data SomeAPIError = SomeAPIError deriving (Show)

instance IsBaseError SomeAPIError

instance IsHTTPError SomeAPIError where
  toErrorCode SomeAPIError = "SOME_API_ERROR"

instance IsAPIError SomeAPIError

instance IsBecknAPIError SomeAPIError

instanceExceptionWithParent 'HTTPException ''SomeAPIError

data SomeBecknAPIError = SomeBecknAPIError deriving (Show)

instance IsBaseError SomeBecknAPIError

instance IsHTTPError SomeBecknAPIError where
  toErrorCode SomeBecknAPIError = "SOME_BECKN_API_ERROR"

instance IsAPIError SomeBecknAPIError

instance IsBecknAPIError SomeBecknAPIError where
  toType SomeBecknAPIError = INTERNAL_ERROR

instanceExceptionWithParent 'HTTPException ''SomeBecknAPIError

instance Metrics.CoreMetrics IO where
  addRequestLatency _ _ _ _ = return ()
  addDatastoreLatency _ _ _ = return ()
  incrementErrorCounter _ _ = return ()
  addUrlCallRetries _ _ = return ()
  addUrlCallRetryFailures _ = return ()
  incrementSortedSetCounter _ = return ()
  incrementStreamCounter _ = return ()
  incrementStreamFailedCounter _ = return ()
  addGenericLatency _ _ = return ()
  incrementSchedulerFailureCounter _ = return ()
  incrementSchedulerJobDisabledCounter _ = return ()
  incrementGenericMetrics _ = return ()
  incrementSystemConfigsFailedCounter _ = return ()
  addGenericLatencyMetrics _ _ = return ()
  addOpenTripPlannerResponse _ _ _ = return ()
  addOpenTripPlannerLatency _ _ _ = return ()
  incrementTryExceptionCounter _ _ = return ()
  incrementProducerError _ = return ()

httpExceptionTests :: TestTree
httpExceptionTests =
  testGroup
    "Endpoint exception catchers tests"
    []
