{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE BangPatterns #-}

module Kernel.Tools.LoopGracefully where

import qualified Control.Exception.Safe as E
import qualified EulerHS.Language as L
import GHC.Conc (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Kernel.Prelude hiding (loop)
import Kernel.Types.App (MonadFlow)
import Kernel.Types.Forkable (Forkable (..))
import Kernel.Utils.Logging (logError, logInfo)
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

-- | State of the graceful loop. Transitions from 'Running' to 'StopRequested'
-- upon receiving SIGINT or SIGTERM.
data LoopState = Running | StopRequested deriving (Eq)

-- | Run a list of monadic actions in an infinite loop, stopping gracefully on
-- SIGINT or SIGTERM. The first action runs on the calling thread; remaining
-- actions are each forked into their own thread via 'awaitableFork'.
--
-- Shutdown semantics:
--
-- * A signal sets the shared 'LoopState' to 'StopRequested'.
-- * Each loop iteration checks the state before executing its action.
-- * When 'StopRequested' is observed the loop exits.
-- * The main thread waits for all forked workers to finish before returning.
-- * Exceptions in any individual loop iteration are logged and the loop
--   continues (unless a stop has been requested).
loopGracefully :: forall m a. (MonadFlow m) => [m a] -> m ()
loopGracefully fns = do
  stop <-
    liftIO do
      stop <- newTVarIO Running
      _ <- installHandler sigINT (Catch $ onSignal "SIGINT" stop) Nothing
      _ <- installHandler sigTERM (Catch $ onSignal "SIGTERM" stop) Nothing
      pure stop
  case fns of
    (fstfn : rest) -> do
      handles <- forM (zip [(1 :: Int) ..] rest) $ \(i, fn) ->
        awaitableFork ("loopGracefully-worker-" <> show i) (loop ("loopGracefully-worker-" <> show i) fn stop)
      loop "loopGracefully-main" fstfn stop
      -- Wait for all forked workers to complete after main loop exits
      forM_ handles $ \h -> do
        void $ E.try @_ @E.SomeException (L.await Nothing h)
    [] -> pure ()

loop :: forall m a. (MonadFlow m) => Text -> m a -> TVar LoopState -> m ()
loop tag fa stop = do
  stopRequested <- liftIO $ readTVarIO stop
  if stopRequested == StopRequested
    then logInfo $ tag <> ": stopping gracefully"
    else do
      result <- E.try @_ @E.SomeException (void (fa >> pure ()))
      case result of
        Left err -> do
          logError $ tag <> ": exception in loop body: " <> show err
          -- Check again before continuing -- a stop may have been requested
          -- during the failed iteration.
          shouldContinue <- liftIO $ readTVarIO stop
          if shouldContinue == StopRequested
            then logInfo $ tag <> ": stopping gracefully after error"
            else loop tag fa stop
        Right _ -> loop tag fa stop

onSignal :: String -> TVar LoopState -> IO ()
onSignal sigName stop = do
  hPutStrLn stderr $ "loopGracefully: received " <> sigName <> ", requesting stop"
  atomically $ writeTVar stop StopRequested
