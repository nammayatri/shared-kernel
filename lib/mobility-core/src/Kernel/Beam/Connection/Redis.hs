module Kernel.Beam.Connection.Redis where

import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Kernel.Beam.Connection.EnvVars as EnvVars
import qualified Kernel.Beam.Connection.Types as ECT
import qualified Kernel.Storage.Hedis.Config as KSHC

prepareRedisConnections :: L.MonadFlow m => ECT.ConnectionConfig -> m ()
prepareRedisConnections ECT.ConnectionConfig {..} = do
  preparePosgreSqlConnection <- L.runIO $ EnvVars.getPreparePosgreSqlConnection
  when preparePosgreSqlConnection (prepareRedisClusterConnection hedisClusterCfg)

kvRedis :: Text
kvRedis = "KVRedis"

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
