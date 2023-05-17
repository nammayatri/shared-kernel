module Kernel.Beam.Utils where

import qualified Database.Beam.Postgres as BP
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Kernel.Beam.Types as KBT

runInReplica :: (L.MonadFlow m) => ((ET.DBConfig BP.Pg) -> m a) -> m a
runInReplica f = do
  dbConf <- L.getOption KBT.PsqlDbCfgR1
  case dbConf of
    Just dbCOnf' -> f dbCOnf'
    Nothing -> L.throwException (KBT.DatabaseError "Db Connection Inforamtion Not Found")
