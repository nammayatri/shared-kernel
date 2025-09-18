module Kernel.Utils.Forkable where

import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (length, map)
import Kernel.Types.Forkable

mapConcurrently :: (L.MonadFlow m, Forkable m) => (a -> m b) -> [a] -> m [b]
mapConcurrently fn ar = do
  awaitables <- mapM (awaitableFork "mapConcurrently" . fn) ar
  results <- rights <$> mapM (L.await Nothing) awaitables
  return results

mapConcurrentlyTagged :: (L.MonadFlow m, Forkable m) => (a -> m b) -> [(Text, a)] -> m [b]
mapConcurrentlyTagged fn ar = do
  awaitables <- mapM (\(tag, x) -> awaitableFork tag (fn x)) ar
  rights <$> mapM (L.await Nothing) awaitables
