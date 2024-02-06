{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Randomizer (randomizerTests) where

import Kernel.Prelude
import Kernel.Randomizer
import Test.Tasty
import Test.Tasty.HUnit

randomizerTests :: TestTree
randomizerTests =
  testGroup
    "getRandomElementUsingPercentages tests"
    [ testGroup
        "Should return error"
        [ emptyElementsError,
          percentageLessThan0Error,
          percentageMoreThan100Error,
          sumNot100Error,
          rangeNotFoundError
        ],
      testGroup
        "Should success"
        [ successBoundaryCase,
          successWithDifferentRandoms
        ]
    ]

mkElements :: [Int] -> [Percentage Char]
mkElements = zipWith (\element percentage -> Percentage {..}) ['a' ..]

emptyElementsError :: TestTree
emptyElementsError = do
  let err = EMPTY_ELEMENTS
  testCase ("Return error: " <> show err) $ do
    let result = getRandomElementUsingPercentages' (mkElements []) 50
    result.pickedElement @?= Left err

percentageLessThan0Error :: TestTree
percentageLessThan0Error = do
  let err = PERCENTAGE_LESS_THAN_0
  testCase ("Return error: " <> show err) $ do
    let result = getRandomElementUsingPercentages' (mkElements [20, 90, -10]) 50
    result.pickedElement @?= Left err

percentageMoreThan100Error :: TestTree
percentageMoreThan100Error = do
  let err = PERCENTAGE_MORE_THAN_100
  testCase ("Return error: " <> show err) $ do
    let result = getRandomElementUsingPercentages' (mkElements [20, 110, 30]) 50
    result.pickedElement @?= Left err

sumNot100Error :: TestTree
sumNot100Error = do
  let err = SUM_NOT_100
  testCase ("Return error: " <> show err) $ do
    let result = getRandomElementUsingPercentages' (mkElements [20, 70, 9]) 50
    result.pickedElement @?= Left err

-- should be 1 <= random <= 100
rangeNotFoundError :: TestTree
rangeNotFoundError = do
  let err = RANGE_NOT_FOUND
  testCase ("Return error: " <> show err) $ do
    let result1 = getRandomElementUsingPercentages' (mkElements [20, 70, 10]) 0
    result1.pickedElement @?= Left err
    let result2 = getRandomElementUsingPercentages' (mkElements [20, 70, 10]) 101
    result2.pickedElement @?= Left err

successBoundaryCase :: TestTree
successBoundaryCase = do
  testCase "Success boundary case: " $ do
    let result1 = getRandomElementUsingPercentages' (mkElements [100]) 55
    result1.pickedElement @?= Right 'a'
    let randoms = [1 .. 100]
    forM_ randoms $ \random -> do
      let result2 = getRandomElementUsingPercentages' (mkElements [100, 0, 0]) random
      result2.pickedElement @?= Right 'a'
    let result3 = getRandomElementUsingPercentages' (mkElements [0, 100, 0]) 1
    result3.pickedElement @?= Right 'b'
    let result4 = getRandomElementUsingPercentages' (mkElements [0, 0, 100]) 12
    result4.pickedElement @?= Right 'c'
    let result5 = getRandomElementUsingPercentages' (mkElements [0, 0, 0, 0, 100, 0, 0]) 88
    result5.pickedElement @?= Right 'e'

successWithDifferentRandoms :: TestTree
successWithDifferentRandoms = do
  let percentages = mkElements [20, 70, 10]
  testCase "Success with different randoms" $ do
    let randoms1 = [1 .. 20]
    forM_ randoms1 $ successTest percentages 'a'

    let randoms2 = [21 .. 90]
    forM_ randoms2 $ successTest percentages 'b'

    let randoms3 = [91 .. 100]
    forM_ randoms3 $ successTest percentages 'c'
  where
    successTest percentages el random = do
      let result = getRandomElementUsingPercentages' percentages random
      result.pickedElement @?= Right el
