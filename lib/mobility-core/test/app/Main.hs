module Main where

import APIExceptions
import Centesimal
import DistanceCalculation
import EulerHS.Prelude
import SignatureAuth
import SlidingWindowLimiter
import Test.Tasty
import Version

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = return $ testGroup "Tests" [unitTests]
  where
    unitTests =
      testGroup
        "Unit tests"
        [ centesimalTests,
          signatureAuthTests,
          httpExceptionTests,
          slidingWindowLimiterTests,
          distanceCalculation,
          readVersionTests
        ]
