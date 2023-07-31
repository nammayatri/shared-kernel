module Kernel.Beam.Connection.Flow where

import qualified EulerHS.Language as L
import qualified Kernel.Beam.Connection.Postgres as PGC
import qualified Kernel.Beam.Connection.Redis as RC
import qualified Kernel.Beam.Connection.Types as ECT
import qualified Kernel.Types.Common as KTC

prepareConnectionDriver :: L.MonadFlow m => ECT.ConnectionConfigDriver -> KTC.Tables -> m ()
prepareConnectionDriver conf tables' = do
  PGC.prepareDBConnectionsDriver conf
  RC.prepareRedisConnectionsDriver conf
  PGC.prepareTables tables'

prepareConnectionRider :: L.MonadFlow m => ECT.ConnectionConfigRider -> KTC.Tables -> m ()
prepareConnectionRider conf tables' = do
  PGC.prepareDBConnectionsRider conf
  RC.prepareRedisConnectionsRider conf
  PGC.prepareTables tables'
