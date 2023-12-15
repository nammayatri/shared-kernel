module Kernel.Beam.Connection.Flow where

import qualified EulerHS.Language as L
import qualified Kernel.Beam.Connection.Postgres as PGC
import qualified Kernel.Beam.Connection.Redis as RC
import qualified Kernel.Beam.Connection.Types as ECT
import Kernel.Prelude

prepareConnectionDriver :: L.MonadFlow m => ECT.ConnectionConfigDriver -> Int -> m ()
prepareConnectionDriver conf kvConfigUpdateFrequency = do
  PGC.prepareDBConnectionsDriver conf
  RC.prepareRedisConnectionsDriver conf
  PGC.setKvConfigUpdateFrequency kvConfigUpdateFrequency
  PGC.setKvConfigMetrics

prepareConnectionRider :: L.MonadFlow m => ECT.ConnectionConfigRider -> Int -> m ()
prepareConnectionRider conf kvConfigUpdateFrequency = do
  PGC.prepareDBConnectionsRider conf
  RC.prepareRedisConnectionsRider conf
  PGC.setKvConfigUpdateFrequency kvConfigUpdateFrequency
  PGC.setKvConfigMetrics
