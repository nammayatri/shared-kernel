module Kernel.Connection.Postgres where

import qualified Data.Text as T
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Kernel.Connection.EnvVars as EnvVars
import qualified Kernel.Connection.Types as ECT
import qualified Kernel.Storage.Esqueleto.Config as KSEC

prepareDBConnections :: L.MonadFlow m => ECT.ConnectionConfig -> m ()
prepareDBConnections ECT.ConnectionConfig {..} = do
  preparePosgreSqlConnection <- L.runIO $ EnvVars.getPreparePosgreSqlConnection
  when preparePosgreSqlConnection (preparePSqlConnection esqDBCfg)

--   when EnvVars.getPrepareRedisClusterConnection prepareRedisClusterConnection

preparePSqlConnection :: L.MonadFlow m => KSEC.EsqDBConfig -> m ()
preparePSqlConnection KSEC.EsqDBConfig {..} = do
  pgConnName <- L.runIO $ EnvVars.postgresConnectionName
  poolConf <- L.runIO postgresPoolConfig
  let psqlDbCfg = ET.mkPostgresPoolConfig pgConnName pgConf poolConf
  ePoolPsql <- L.initSqlDBConnection psqlDbCfg
  L.setOption Extra.EulerPsqlDbCfg psqlDbCfg
  L.throwOnFailedWithLog ePoolPsql L.SqlDBConnectionFailedException "Failed to connect to PSQL DB."
  where
    pgConf =
      ET.PostgresConfig
        { connectHost = T.unpack connectHost,
          connectPort = connectPort,
          connectUser = T.unpack connectUser,
          connectPassword = T.unpack connectPassword,
          connectDatabase = T.unpack connectDatabase
        }

postgresPoolConfig :: IO ET.PoolConfig
postgresPoolConfig = do
  stripes <- EnvVars.getMysqlPoolStripes
  keepAlive <- fromInteger <$> EnvVars.getMysqlPoolIdleTime
  resourcesPerStripe <- EnvVars.getMysqlPoolMax
  pure $ ET.PoolConfig {..}
