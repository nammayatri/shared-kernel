module Kernel.Utils.Forkable where

import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (length, map)
import EulerHS.Types (AwaitingError (..), Microseconds (..))
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Forkable
import Kernel.Utils.Error.Throwing (throwError)
import Kernel.Utils.Logging (Log, logInfo)

mapConcurrently :: (L.MonadFlow m, Forkable m) => (a -> m b) -> [a] -> m [b]
mapConcurrently fn ar = do
  awaitables <- mapM (awaitableFork "mapConcurrently" . fn) ar
  results <- rights <$> mapM (L.await Nothing) awaitables
  return results

mapConcurrentlyTagged :: (L.MonadFlow m, Forkable m) => (a -> m b) -> [(Text, a)] -> m [b]
mapConcurrentlyTagged fn ar = do
  awaitables <- mapM (\(tag, x) -> awaitableFork tag (fn x)) ar
  rights <$> mapM (L.await Nothing) awaitables

-- Runs each provider on a fresh EulerHS fork with a per-call timeout. On
-- timeout the caller stops waiting and moves on — EulerHS exposes no
-- cancellation primitive, so the forked flow keeps running in the background
-- and its result is discarded. Provider actions must be safe to leave running
-- (typically HTTP calls whose manager has its own responseTimeout).
runWithFallbackAndTimeout ::
  (L.MonadFlow m, Forkable m, Log m, MonadThrow m) =>
  Text ->
  [a] ->
  Int ->
  (b -> Bool) ->
  (a -> m b) ->
  m b
runWithFallbackAndTimeout tag providers timeoutInSec isSuccess action = go providers
  where
    go [] =
      throwError $ InternalError (tag <> ": all configured providers exhausted (timeout/failure)")
    go (provider : rest) = do
      awaitable <- awaitableFork tag (action provider)
      L.await (Just (Microseconds (fromIntegral timeoutInSec * 1000000))) awaitable >>= \case
        Right result
          | isSuccess result -> pure result
          | otherwise -> logInfo (tag <> ": provider returned unsuccessful result; falling back") >> go rest
        Left AwaitingTimeout -> logInfo (tag <> ": provider timed out after " <> show timeoutInSec <> "s; falling back") >> go rest
        Left (ForkedFlowError e) -> logInfo (tag <> ": provider failed (" <> e <> "); falling back") >> go rest
