{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Main where

import APIExceptions
import Centesimal
import DistanceCalculation
import EulerHS.Prelude
import Randomizer
import SignatureAuth
import SlidingWindowLimiter
import SnippetsCheck (snippetsCheckTests)
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
          readVersionTests,
          snippetsCheckTests,
          randomizerTests
        ]
