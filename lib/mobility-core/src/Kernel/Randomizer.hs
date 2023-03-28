{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Randomizer where

import Safe (at)
import System.Random
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
