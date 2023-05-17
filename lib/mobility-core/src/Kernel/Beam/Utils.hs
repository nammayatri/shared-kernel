module Kernel.Beam.Utils where

import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Kernel.Beam.Types as KBT

runInReplica :: (L.MonadFlow m) => (KBT.BeamFlow m a) -> (m a)
runInReplica f = do
  dbConf <- L.getOption KBT.PsqlDbCfgR1
  case dbConf of
    Just dbCOnf' -> runReaderT f (KBT.BeamState dbCOnf')
    Nothing -> L.throwException (KBT.DatabaseError "Db Connection Inforamtion Not Found")
