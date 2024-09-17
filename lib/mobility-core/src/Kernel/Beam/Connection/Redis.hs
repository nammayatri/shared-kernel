module Kernel.Beam.Connection.Redis where

import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Kernel.Beam.Connection.EnvVars as EnvVars
import qualified Kernel.Beam.Connection.Types as ECT
import qualified Kernel.Storage.Hedis.Config as KSHC

prepareRedisConnectionsDriver :: L.MonadFlow m => ECT.ConnectionConfigDriver -> m ()
prepareRedisConnectionsDriver ECT.ConnectionConfigDriver {..} = do
  preparePosgreSqlConnection <- L.runIO EnvVars.getPreparePosgreSqlConnection
  when preparePosgreSqlConnection (prepareRedisClusterConnection hedisClusterCfg >> prepareRedisReplicaConnection hedisClusterCfg)

prepareRedisConnectionsRider :: L.MonadFlow m => ECT.ConnectionConfigRider -> m ()
prepareRedisConnectionsRider ECT.ConnectionConfigRider {..} = do
  preparePosgreSqlConnection <- L.runIO EnvVars.getPreparePosgreSqlConnection
  when preparePosgreSqlConnection (prepareRedisClusterConnection hedisClusterCfg >> prepareRedisReplicaConnection hedisClusterCfg)

prepareRedisConnectionsDashboard :: L.MonadFlow m => ECT.ConnectionConfigDashboard -> m ()
prepareRedisConnectionsDashboard ECT.ConnectionConfigDashboard {..} = do
  preparePosgreSqlConnection <- L.runIO EnvVars.getPreparePosgreSqlConnection
  when preparePosgreSqlConnection (prepareRedisClusterConnection hedisClusterCfg >> prepareRedisReplicaConnection hedisClusterCfg)

kvRedis :: Text
kvRedis = "KVRedis"

kvReplicaRedis :: Text
kvReplicaRedis = "KVReplicaRedis"

prepareRedisClusterConnection :: (HasCallStack, L.MonadFlow m) => KSHC.HedisCfg -> m ()
prepareRedisClusterConnection KSHC.HedisCfg {..} = do
  L.logDebug @Text "prepareDBConnections" ("kvdbConfig:" <> show hedisClusterConf)
  kvRedisCon <- L.initKVDBConnection (ET.mkKVDBClusterConfig kvRedis hedisClusterConf)
  L.throwOnFailedWithLog kvRedisCon L.KVDBConnectionFailedException "Failed to connect to Redis Cluster DB."
  where
    hedisClusterConf =
      ET.RedisConfig
        { connectHost = connectHost,
          connectPort = connectPort,
          connectAuth = connectAuth,
          connectDatabase = connectDatabase,
          connectMaxConnections = connectMaxConnections,
          connectReadOnly = False,
          connectMaxIdleTime = connectMaxIdleTime,
          connectTimeout = connectTimeout
        }

prepareRedisReplicaConnection :: (HasCallStack, L.MonadFlow m) => KSHC.HedisCfg -> m ()
prepareRedisReplicaConnection KSHC.HedisCfg {..} = do
  L.logDebug @Text "prepareDBConnections" ("kvdbConfig:" <> show hedisClusterConf)
  kvRedisCon <- L.initKVDBConnection (ET.mkKVDBClusterConfig kvReplicaRedis hedisClusterConf)
  L.throwOnFailedWithLog kvRedisCon L.KVDBConnectionFailedException "Failed to connect to Redis Cluster DB."
  where
    hedisClusterConf =
      ET.RedisConfig
        { connectHost = connectHost,
          connectPort = connectPort,
          connectAuth = connectAuth,
          connectDatabase = connectDatabase,
          connectMaxConnections = connectMaxConnections,
          connectReadOnly = True,
          connectMaxIdleTime = connectMaxIdleTime,
          connectTimeout = connectTimeout
        }
