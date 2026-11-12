module Kernel.Beam.Connection.Redis where

import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Kernel.Beam.Connection.EnvVars as EnvVars
import qualified Kernel.Beam.Connection.Types as ECT
import qualified Kernel.Beam.Types as KBT
import qualified Kernel.Storage.Hedis.Config as KSHC
import Kernel.Types.Version (CloudType, cloudTypeCode)
import System.IO.Error (ioError, userError)

prepareRedisConnectionsDriver :: L.MonadFlow m => ECT.ConnectionConfigDriver -> m ()
prepareRedisConnectionsDriver ECT.ConnectionConfigDriver {..} = do
  prepareRedisConnection <- L.runIO EnvVars.getPrepareRedisClusterConnection
  when prepareRedisConnection $ do
    prepareKvRedisClouds hedisClusterCfgs

prepareRedisConnectionsRider :: L.MonadFlow m => ECT.ConnectionConfigRider -> m ()
prepareRedisConnectionsRider ECT.ConnectionConfigRider {..} = do
  prepareRedisConnection <- L.runIO EnvVars.getPrepareRedisClusterConnection
  when prepareRedisConnection $ do
    prepareKvRedisClouds hedisClusterCfgs

prepareRedisConnectionsDashboard :: L.MonadFlow m => ECT.ConnectionConfigDashboard -> m ()
prepareRedisConnectionsDashboard ECT.ConnectionConfigDashboard {..} = do
  prepareRedisConnection <- L.runIO EnvVars.getPrepareRedisClusterConnection
  when prepareRedisConnection $ do
    prepareKvRedisClouds hedisClusterCfgs

kvRedis :: Text
kvRedis = "KVRedis"

kvRedisCloudConnName :: CloudType -> Text
kvRedisCloudConnName cloud = "KVRedis" <> show cloud

prepareKvRedisClouds :: (HasCallStack, L.MonadFlow m) => [KSHC.HedisCloudCfg] -> m ()
prepareKvRedisClouds clouds = do
  (localCfg, peerCfgs) <- either (L.runIO . ioError . userError) pure (KSHC.splitLocalCloud clouds)
  prepareNamedRedisClusterConnection kvRedis localCfg.hedisCloudCfg
  forM_ peerCfgs $ \peer -> prepareNamedRedisClusterConnection (kvRedisCloudConnName peer.hedisCloud) peer.hedisCloudCfg
  let entryFor name cloudCfg = (,name) <$> cloudTypeCode cloudCfg.hedisCloud
  L.setOption KBT.KvRedisClouds $
    KBT.KvRedisCloudsCfg
      { cloudCodes = catMaybes (entryFor kvRedis localCfg : map (\peer -> entryFor (kvRedisCloudConnName peer.hedisCloud) peer) peerCfgs)
      }

prepareNamedRedisClusterConnection :: (HasCallStack, L.MonadFlow m) => Text -> KSHC.HedisCfg -> m ()
prepareNamedRedisClusterConnection connName KSHC.HedisCfg {..} = do
  L.logDebug @Text "prepareNamedRedisClusterConnection" ("kvdbConfig " <> connName <> ":" <> show hedisClusterConf)
  kvRedisCon <- L.initKVDBConnection (ET.mkKVDBClusterConfig connName hedisClusterConf)
  L.throwOnFailedWithLog kvRedisCon L.KVDBConnectionFailedException ("Failed to connect to Redis Cluster DB " <> connName)
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
