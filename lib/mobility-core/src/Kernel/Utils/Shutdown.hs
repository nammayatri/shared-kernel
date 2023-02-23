 {-
  Copyright 2022-23, Juspay India Pvt Ltd
  
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
  
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is 
  
  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
  
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero 
  
  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Shutdown where

import Control.Concurrent.STM.TMVar
import GHC.Conc
import Kernel.Prelude
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

type Shutdown = TMVar ()

handleShutdown :: Shutdown -> IO () -> IO () -> IO ()
handleShutdown shutdown onShutdown closeSocket = do
  void $ installHandler sigTERM (Catch $ shutdownAction "sigTERM") Nothing
  void $ installHandler sigINT (Catch $ shutdownAction "sigINT") Nothing
  where
    shutdownAction reason = do
      isLocked <- atomically $ do
        isEmptyTMVar shutdown >>= \case
          True -> do
            putTMVar shutdown ()
            return True
          False -> return False
      when isLocked $ do
        putStrLn ("Shutting down by " <> reason :: Text)
      onShutdown
      closeSocket

waitForShutdown :: Shutdown -> IO ()
waitForShutdown = atomically . takeTMVar

mkShutdown :: IO Shutdown
mkShutdown = newEmptyTMVarIO

untilShutdown ::
  ( MonadIO m,
    MonadReader r m,
    HasField "isShuttingDown" r Shutdown
  ) =>
  m () ->
  m ()
untilShutdown =
  whileM isRunning

isRunning ::
  ( MonadIO m,
    MonadReader r m,
    HasField "isShuttingDown" r Shutdown
  ) =>
  m Bool
isRunning = liftIO . atomically . isEmptyTMVar =<< asks (.isShuttingDown)
