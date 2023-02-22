 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Centesimal where

import Data.Aeson
import EulerHS.Prelude
import Kernel.Types.Centesimal (Centesimal (..))
import Test.Tasty
import Test.Tasty.HUnit

jsonParsing :: TestTree
jsonParsing = testCase "Parsing JSON with 2 digits precision" $ do
  let result = fromJSON (Number 1.25) :: Result Centesimal
  case result of
    Error err -> assertFailure $ "Parsing of \"1.25\" should succeed, but encountered error: " <> err
    Success x -> x @?= 1.25

jsonParsingWithZeros :: TestTree
jsonParsingWithZeros = testCase "Parsing JSON with trailing zeros (1.250000)" $ do
  let result = fromJSON (Number 1.250000) :: Result Centesimal
  case result of
    Error err -> assertFailure $ "Parsing of \"1.250000\" should succeed, but encountered error: " <> err
    Success x -> x @?= 1.25

jsonParsingRoundingUp :: TestTree
jsonParsingRoundingUp = testCase "Parsing JSON with rounding to upper (1.259999 should be 1.26)" $ do
  let result = fromJSON (Number 1.259999) :: Result Centesimal
  case result of
    Error err -> assertFailure $ "Parsing of \"1.259999\" should succeed, but encountered error: " <> err
    Success x -> x @?= 1.26

jsonParsingRoundingDown :: TestTree
jsonParsingRoundingDown = testCase "Parsing JSON with rounding to lower (1.251111 should be 1.25)" $ do
  let result = fromJSON (Number 1.251111) :: Result Centesimal
  case result of
    Error err -> assertFailure $ "Parsing of \"1.251111\" should succeed, but encountered error: " <> err
    Success x -> x @?= 1.25

jsonParsingNegativeRoundingUp :: TestTree
jsonParsingNegativeRoundingUp = testCase "Parsing JSON with rounding to upper (-1.251111 should be -1.25)" $ do
  let result = fromJSON (Number (negate 1.251111)) :: Result Centesimal
  case result of
    Error err -> assertFailure $ "Parsing of \"-1.251111\" should succeed, but encountered error: " <> err
    Success x -> x @?= -1.25

jsonParsingNegativeRoundingDown :: TestTree
jsonParsingNegativeRoundingDown = testCase "Parsing JSON with rounding to lower (-1.259999 should be -1.26)" $ do
  let result = fromJSON (Number (negate 1.259999)) :: Result Centesimal
  case result of
    Error err -> assertFailure $ "Parsing of \"-1.259999\" should succeed, but encountered error: " <> err
    Success x -> x @?= -1.26

jsonParsingRoundingUpFiveHundredth :: TestTree
jsonParsingRoundingUpFiveHundredth = testCase "Parsing JSON with rounding to upper having five hundredth (1.255 should be 1.26)" $ do
  let result = fromJSON (Number 1.255) :: Result Centesimal
  case result of
    Error err -> assertFailure $ "Parsing of \"1.255\" should succeed, but encountered error: " <> err
    Success x -> x @?= 1.26

jsonParsingNegativeRoundingDownFiveHundredth :: TestTree
jsonParsingNegativeRoundingDownFiveHundredth = testCase "Parsing JSON with rounding to upper having five hundredth (-1.255 should be -1.26)" $ do
  let result = fromJSON (Number (-1.255)) :: Result Centesimal
  case result of
    Error err -> assertFailure $ "Parsing of \"1.255\" should succeed, but encountered error: " <> err
    Success x -> x @?= (-1.26)

convertingFromDouble :: TestTree
convertingFromDouble = testCase "Converting from Double" $ do
  let result :: Centesimal = realToFrac (-1.25 :: Double)
  result @?= -1.25

convertingFromDoubleWithZeros :: TestTree
convertingFromDoubleWithZeros = testCase "Converting from Double with trailing zeros (-1.250000)" $ do
  let result :: Centesimal = realToFrac (-1.250000 :: Double)
  result @?= -1.25

convertingFromDoubleRoundingUp :: TestTree
convertingFromDoubleRoundingUp = testCase "Converting from Double with rounding to upper (1.2599999 should be 1.26)" $ do
  let result :: Centesimal = realToFrac (1.2599999 :: Double)
  result @?= 1.26

convertingFromDoubleRoundingDown :: TestTree
convertingFromDoubleRoundingDown = testCase "Converting from Double with rounding to lower (1.251 should be 1.25)" $ do
  let result :: Centesimal = realToFrac (1.251 :: Double)
  result @?= 1.25

convertingFromDoubleRoundingUpFiveHundredth :: TestTree
convertingFromDoubleRoundingUpFiveHundredth = testCase "Converting from Double with rounding to upper having five hundredth (1.255 should be 1.26)" $ do
  let result :: Centesimal = realToFrac (1.255 :: Double)
  result @?= 1.26

convertingFromDoubleRoundingDownFiveHundredthNegative :: TestTree
convertingFromDoubleRoundingDownFiveHundredthNegative = testCase "Converting from Double with rounding to upper having five hundredth negative (-1.255 should be -1.26)" $ do
  let result :: Centesimal = realToFrac ((-1.255) :: Double)
  result @?= (-1.26)

centesimalTests :: TestTree
centesimalTests =
  testGroup
    "Centi tests"
    [ jsonParsing,
      jsonParsingWithZeros,
      jsonParsingRoundingUp,
      jsonParsingRoundingDown,
      jsonParsingNegativeRoundingUp,
      jsonParsingNegativeRoundingDown,
      jsonParsingRoundingUpFiveHundredth,
      jsonParsingNegativeRoundingDownFiveHundredth,
      convertingFromDouble,
      convertingFromDoubleWithZeros,
      convertingFromDoubleRoundingUp,
      convertingFromDoubleRoundingDown,
      convertingFromDoubleRoundingUpFiveHundredth,
      convertingFromDoubleRoundingDownFiveHundredthNegative
    ]
