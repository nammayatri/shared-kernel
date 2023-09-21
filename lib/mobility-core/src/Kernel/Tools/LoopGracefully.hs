{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.Tools.LoopGracefully where

import GHC.Conc (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Kernel.Prelude hiding (loop)
import Kernel.Types.App (MonadFlow)
import Kernel.Types.Common (fork)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

loopGracefully :: forall m a. (MonadFlow m) => [m a] -> m ()
loopGracefully fns = do
  stop <-
    liftIO do
      stop <- newTVarIO 1
      hSetBuffering stdout NoBuffering
      _ <- installHandler sigINT (Catch $ onSigInt stop) Nothing
      _ <- installHandler sigTERM (Catch $ onSigTerm stop) Nothing
      pure stop
  case fns of
    (fstfn : rest) -> do
      mapM_ (\fn -> fork "" $ loop fn stop) rest
      loop fstfn stop
    [] -> pure ()

loop :: forall m a. (MonadFlow m) => m a -> TVar Int -> m ()
loop fa stop = do
  stopRequested :: Int <- liftIO $ readTVarIO stop
  if stopRequested > 1
    then print ("bye!" :: String)
    else do
      void fa
      loop fa stop

onSigInt :: TVar Int -> IO ()
onSigInt stop = do
  print ("got sigINT" :: String)
  atomically $ writeTVar stop 2

onSigTerm :: TVar Int -> IO ()
onSigTerm stop = do
  print ("got sigTERM" :: String)
  atomically $ writeTVar stop 2
