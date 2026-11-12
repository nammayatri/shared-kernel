module Kernel.Beam.Connection.Redis where

import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Kernel.Beam.Connection.EnvVars as EnvVars
import qualified Kernel.Beam.Connection.Types as ECT
import qualified Kernel.Beam.Types as KBT
import qualified Kernel.Storage.Hedis.Config as KSHC
import Kernel.Types.CloudTag (localCloudCode)
import Kernel.Types.Version (CloudType (..), cloudTypeCode)

prepareRedisConnectionsDriver :: L.MonadFlow m => ECT.ConnectionConfigDriver -> m ()
prepareRedisConnectionsDriver ECT.ConnectionConfigDriver {..} = do
  prepareRedisConnection <- L.runIO EnvVars.getPrepareRedisClusterConnection
  when prepareRedisConnection $ do
    prepareKvRedisClouds hedisClusterCfg hedisSecondaryClusterCfg hedisPeerClusterCfgs

prepareRedisConnectionsRider :: L.MonadFlow m => ECT.ConnectionConfigRider -> m ()
prepareRedisConnectionsRider ECT.ConnectionConfigRider {..} = do
  prepareRedisConnection <- L.runIO EnvVars.getPrepareRedisClusterConnection
  when prepareRedisConnection $ do
    prepareKvRedisClouds hedisClusterCfg hedisSecondaryClusterCfg hedisPeerClusterCfgs

prepareRedisConnectionsDashboard :: L.MonadFlow m => ECT.ConnectionConfigDashboard -> m ()
prepareRedisConnectionsDashboard ECT.ConnectionConfigDashboard {..} = do
  prepareRedisConnection <- L.runIO EnvVars.getPrepareRedisClusterConnection
  when prepareRedisConnection $ do
    prepareKvRedisClouds hedisClusterCfg hedisSecondaryClusterCfg hedisPeerClusterCfgs

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

kvRedisPeerConnName :: CloudType -> Text
kvRedisPeerConnName cloud = "KVRedis" <> show cloud

prepareKvRedisClouds :: (HasCallStack, L.MonadFlow m) => KSHC.HedisCfg -> KSHC.HedisCfg -> [KSHC.HedisPeerCfg] -> m ()
prepareKvRedisClouds primaryCfg legacySecondaryCfg peers = do
  prepareRedisClusterConnection primaryCfg
  let localEntry = (,kvRedis) <$> localCloudCode
  case peers of
    [] -> do
      prepareSecondaryRedisClusterConnection legacySecondaryCfg
      let legacyPeerCode = case localCloudCode of
            Just 0 -> cloudTypeCode GCP
            Just 1 -> cloudTypeCode AWS
            _ -> Nothing
      L.setOption KBT.KvRedisClouds $
        KBT.KvRedisCloudsCfg
          { peerConns = [],
            cloudCodes = catMaybes [localEntry, (,kvRedisSecondary) <$> legacyPeerCode]
          }
    firstPeer : restPeers -> do
      prepareSecondaryRedisClusterConnection firstPeer.peerHedisCfg
      forM_ restPeers $ \peer -> prepareNamedRedisClusterConnection (kvRedisPeerConnName peer.peerCloud) peer.peerHedisCfg
      let entryFor name peer = (,name) <$> cloudTypeCode peer.peerCloud
      L.setOption KBT.KvRedisClouds $
        KBT.KvRedisCloudsCfg
          { peerConns = map (kvRedisPeerConnName . (.peerCloud)) restPeers,
            cloudCodes = catMaybes (localEntry : entryFor kvRedisSecondary firstPeer : map (\peer -> entryFor (kvRedisPeerConnName peer.peerCloud) peer) restPeers)
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
