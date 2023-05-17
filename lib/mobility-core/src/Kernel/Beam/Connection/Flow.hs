module Kernel.Beam.Connection.Flow where

import qualified EulerHS.Language as L
import qualified Kernel.Beam.Connection.Postgres as PGC
import qualified Kernel.Beam.Connection.Redis as RC
import qualified Kernel.Beam.Connection.Types as ECT

prepareConnection :: L.MonadFlow m => ECT.ConnectionConfig -> m ()
prepareConnection conf = do
  PGC.prepareDBConnections conf
  RC.prepareRedisConnections conf
