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

import Control.Arrow (left)
import qualified Data.Aeson as A
import Data.Maybe (fromJust)
import EulerHS.Prelude
import qualified Kernel.Beam.ART.ARTUtils as ART
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.Error.FlowHandling
import qualified Servant as S
import Servant.Client.Core
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
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
  addGenericLatency _ _ = return ()
  incrementSchedulerFailureCounter _ = return ()
  incrementGenericMetrics _ = return ()
  incrementSystemConfigsFailedCounter _ = return ()
  logApiResponseData _ _ = return ()
  getIsArtReplayerEnabled = return False
  getArtReplayResponse _ = return $ Left $ DecodeFailure ("Error occured during ART replay" :: Text) ART.exampleResponse

httpExceptionTests :: TestTree
httpExceptionTests =
  testGroup
    "Endpoint exception catchers tests"
    [ testGroup
        "Throwing any error in our endpoints must return HTTPError"
        [ apiErrorInEndpoint,
          becknApiErrorInEndpoint,
          someErrorInEndpoint
        ],
      testGroup
        "Throwing any error in Beckn endpoints must return BecknAPIError"
        [ apiErrorInBecknEndpoint,
          becknApiErrorInBecknEndpoint,
          someErrorInBecknEndpoint
        ]
    ]

apiErrorInEndpoint :: TestTree
apiErrorInEndpoint =
  testCase "Throwing some Domain error" $
    mustThrow @APIError $ apiHandler (throwM SomeAPIError)

becknApiErrorInEndpoint :: TestTree
becknApiErrorInEndpoint =
  testCase "Throwing some Beckn API error" $
    mustThrow @APIError $ apiHandler (throwM SomeBecknAPIError)

someErrorInEndpoint :: TestTree
someErrorInEndpoint =
  testCase "Throwing SomeException" $
    mustThrow @APIError $ apiHandler (error "Some error")

apiErrorInBecknEndpoint :: TestTree
apiErrorInBecknEndpoint =
  testCase "Throwing some Domain error" $
    mustThrow @BecknAPIError $ becknApiHandler (throwM SomeAPIError)

becknApiErrorInBecknEndpoint :: TestTree
becknApiErrorInBecknEndpoint =
  testCase "Throwing some Beckn API error" $
    mustThrow @BecknAPIError $ becknApiHandler (throwM SomeBecknAPIError)

someErrorInBecknEndpoint :: TestTree
someErrorInBecknEndpoint =
  testCase "Throwing SomeException" $
    mustThrow @BecknAPIError $ becknApiHandler (error "Some error")

mustThrow :: forall (e :: Type). (Show e, FromJSON e) => IO () -> IO ()
mustThrow flow = try flow >>= (`shouldSatisfy` isLeft) . serverErrorTo @e

serverErrorTo :: FromJSON a => Either S.ServerError () -> Either a ()
serverErrorTo = left (fromJust . A.decode . S.errBody)
