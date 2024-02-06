{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Kernel.Randomizer where

import Kernel.Prelude (listToMaybe)
import Safe (at)
import System.Random hiding (random)
import qualified Text.Show
import Universum

getRandomInRange :: (MonadIO m, Random a) => (a, a) -> m a
getRandomInRange = liftIO . randomRIO

randomizeList ::
  forall m arr1 arr2 a.
  ( Element (arr1 a) ~ a,
    MonadIO m,
    Applicative arr2,
    Monoid (arr2 a),
    Container (arr1 a)
  ) =>
  arr1 a ->
  m (arr2 a)
randomizeList = randomizeList' . toList
  where
    randomizeList' [] = return mempty
    randomizeList' l = do
      let len = length l
      randNum <- getRandomInRange (0, len - 1)
      let (leftPart, el : rightPart) = splitAt randNum l
      (pure el <>) <$> randomizeList' (leftPart <> rightPart)

getRandomElement :: (Element (arr a) ~ a, MonadIO m, Container (arr a)) => arr a -> m a
getRandomElement arr = do
  let len = length arr
  randNum <- getRandomInRange (0, len - 1)
  return $ toList arr `at` randNum

-- get value from list using provided percentages

data Percentage e = Percentage
  { element :: e,
    percentage :: Int
  }

data PercentagesResult e = PercentagesResult
  { random :: Int,
    pickedElement :: Either RangeError e,
    ranges :: [PercentageRange e]
  }
  deriving (Show)

data PercentageRange e = PercentageRange
  { element :: e,
    percentage :: Int,
    range :: Range
  }
  deriving (Show)

data Range = Range
  { rangeFloor :: Int,
    rangeCeil :: Int
  }

instance Show Range where
  show range = show range.rangeFloor <> " < random <= " <> show range.rangeCeil

-- RANGE_NOT_FOUND should never happen
data RangeError
  = EMPTY_ELEMENTS
  | PERCENTAGE_LESS_THAN_0
  | PERCENTAGE_MORE_THAN_100
  | SUM_NOT_100
  | RANGE_NOT_FOUND
  deriving (Show, Eq)

getRandomElementUsingPercentages :: MonadIO m => [Percentage e] -> m (PercentagesResult e)
getRandomElementUsingPercentages percentages = do
  random <- getRandomInRange (1, 100)
  pure $ getRandomElementUsingPercentages' percentages random

getRandomElementUsingPercentages' :: [Percentage e] -> Int -> PercentagesResult e
getRandomElementUsingPercentages' percentages random
  | null percentages = PercentagesResult {pickedElement = Left EMPTY_ELEMENTS, ranges = [], random}
  | any (\p -> p.percentage < 0) percentages = PercentagesResult {pickedElement = Left PERCENTAGE_LESS_THAN_0, ranges = [], random}
  | any (\p -> p.percentage > 100) percentages = PercentagesResult {pickedElement = Left PERCENTAGE_MORE_THAN_100, ranges = [], random}
  | sum (map (.percentage) percentages) /= 100 = PercentagesResult {pickedElement = Left SUM_NOT_100, ranges = [], random}
  | otherwise = do
    let emptyResult = PercentagesResult {pickedElement = Left RANGE_NOT_FOUND, ranges = [], random}
    foldl foldRange emptyResult percentages
  where
    foldRange :: PercentagesResult e -> Percentage e -> PercentagesResult e
    foldRange result percentage = do
      let range = maybe (Range 0 0) (.range) (listToMaybe result.ranges)
          range' = Range {rangeFloor = range.rangeCeil, rangeCeil = range.rangeCeil + percentage.percentage}
      let percentageRange' =
            PercentageRange
              { element = percentage.element,
                percentage = percentage.percentage,
                range = range'
              }
      let pickedElement' = case result.pickedElement of
            Left err -> if range'.rangeFloor < random && random <= range'.rangeCeil then Right percentage.element else Left err
            Right pickedElement -> Right pickedElement
      PercentagesResult
        { random,
          pickedElement = pickedElement',
          ranges = percentageRange' : result.ranges
        }
