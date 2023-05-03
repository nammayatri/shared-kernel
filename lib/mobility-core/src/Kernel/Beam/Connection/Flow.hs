module Kernel.Beam.Connection.Flow where

import qualified EulerHS.Language as L
import qualified Kernel.Beam.Connection.Postgres as PGC
import qualified Kernel.Beam.Connection.Redis as RC
import qualified Kernel.Beam.Connection.Types as ECT
import qualified Kernel.Types.Common as KTC

prepareConnection :: L.MonadFlow m => ECT.ConnectionConfig -> KTC.Tables -> m ()
prepareConnection conf tables' = do
  PGC.prepareDBConnections conf
  RC.prepareRedisConnections conf
  PGC.prepareTables tables'
