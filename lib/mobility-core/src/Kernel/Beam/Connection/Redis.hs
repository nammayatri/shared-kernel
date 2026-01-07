module Kernel.Beam.Connection.Redis where

import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Kernel.Beam.Connection.EnvVars as EnvVars
import qualified Kernel.Beam.Connection.Types as ECT
import qualified Kernel.Storage.Hedis.Config as KSHC

prepareRedisConnectionsDriver :: L.MonadFlow m => ECT.ConnectionConfigDriver -> m ()
prepareRedisConnectionsDriver ECT.ConnectionConfigDriver {..} = do
  prepareRedisConnection <- L.runIO EnvVars.getPrepareRedisClusterConnection
  when prepareRedisConnection $ do
    prepareRedisClusterConnection hedisClusterCfg
    prepareSecondaryRedisClusterConnection hedisSecondaryClusterCfg

prepareRedisConnectionsRider :: L.MonadFlow m => ECT.ConnectionConfigRider -> m ()
prepareRedisConnectionsRider ECT.ConnectionConfigRider {..} = do
  prepareRedisConnection <- L.runIO EnvVars.getPrepareRedisClusterConnection
  when prepareRedisConnection $ do
    prepareRedisClusterConnection hedisClusterCfg
    prepareSecondaryRedisClusterConnection hedisSecondaryClusterCfg

prepareRedisConnectionsDashboard :: L.MonadFlow m => ECT.ConnectionConfigDashboard -> m ()
prepareRedisConnectionsDashboard ECT.ConnectionConfigDashboard {..} = do
  prepareRedisConnection <- L.runIO EnvVars.getPrepareRedisClusterConnection
  when prepareRedisConnection $ do
    prepareRedisClusterConnection hedisClusterCfg
    prepareSecondaryRedisClusterConnection hedisSecondaryClusterCfg

kvRedis :: Text
kvRedis = "KVRedis"

-- Secondary cloud Redis connection name
kvRedisSecondary :: Text
kvRedisSecondary = "KVRedisSecondary"

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
          connectReadOnly = connectReadOnly,
          connectMaxIdleTime = connectMaxIdleTime,
          connectTimeout = connectTimeout
        }

-- Secondary Cloud (e.g., GCP) Redis Connection
prepareSecondaryRedisClusterConnection :: (HasCallStack, L.MonadFlow m) => KSHC.HedisCfg -> m ()
prepareSecondaryRedisClusterConnection KSHC.HedisCfg {..} = do
  L.logDebug @Text "prepareSecondaryRedisClusterConnection" ("kvdbConfigSecondary:" <> show hedisClusterConf)
  kvRedisCon <- L.initKVDBConnection (ET.mkKVDBClusterConfig kvRedisSecondary hedisClusterConf)
  L.throwOnFailedWithLog kvRedisCon L.KVDBConnectionFailedException "Failed to connect to Secondary Redis Cluster DB."
  where
    hedisClusterConf =
      ET.RedisConfig
        { connectHost = connectHost,
          connectPort = connectPort,
          connectAuth = connectAuth,
          connectDatabase = connectDatabase,
          connectMaxConnections = connectMaxConnections,
          connectReadOnly = connectReadOnly,
          connectMaxIdleTime = connectMaxIdleTime,
          connectTimeout = connectTimeout
        }
