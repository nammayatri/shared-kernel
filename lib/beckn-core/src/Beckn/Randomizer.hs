module Beckn.Randomizer where

import System.Random
import Universum

getRandom :: (MonadIO m, Random a) => m a
getRandom = liftIO randomIO

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
