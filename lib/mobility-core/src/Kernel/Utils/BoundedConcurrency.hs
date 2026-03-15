{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Bounded concurrency helpers for fan-out operations such as batch notification sends.
--
-- Instead of sending requests sequentially (one at a time) or all at once
-- (potentially overwhelming the downstream service), these helpers run up to @n@
-- operations concurrently using a semaphore.
module Kernel.Utils.BoundedConcurrency
  ( forConcurrentlyBounded_,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import qualified Control.Exception as Exception
import Kernel.Prelude

-- | Execute an IO action for each element of the list with at most @n@
-- actions running concurrently.  All actions run to completion (or failure)
-- before this function returns.
--
-- This is useful for fan-out to external services (e.g. FCM batch sends)
-- where unbounded concurrency would saturate the downstream.
--
-- @
-- forConcurrentlyBounded_ 10 recipients $ \\recipient ->
--   sendNotification config recipient
-- @
forConcurrentlyBounded_ :: (MonadIO m) => Int -> [a] -> (a -> IO ()) -> m ()
forConcurrentlyBounded_ n xs f = liftIO $ do
  sem <- newQSem n
  mvars <- forM xs $ \x -> do
    mvar <- newEmptyMVar
    _ <- forkIO $ do
      waitQSem sem
      Exception.finally (f x) (signalQSem sem)
        `Exception.catch` (\(e :: SomeException) -> putMVar mvar (Just e) >> Exception.throwIO e)
      putMVar mvar Nothing
    pure mvar
  forM_ mvars $ \mvar -> do
    mbErr <- takeMVar mvar
    case mbErr of
      Nothing -> pure ()
      Just _ -> pure () -- errors are already logged/handled by individual sends
