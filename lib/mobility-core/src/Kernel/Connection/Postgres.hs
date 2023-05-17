module Kernel.Connection.Postgres where

import qualified Data.Text as T
import qualified Database.Beam.Postgres as BP
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Kernel.Beam.Types as KBT
import qualified Kernel.Connection.EnvVars as EnvVars
import qualified Kernel.Connection.Types as ECT
import qualified Kernel.Storage.Esqueleto.Config as KSEC

prepareDBConnections :: L.MonadFlow m => ECT.ConnectionConfig -> m ()
prepareDBConnections ECT.ConnectionConfig {..} = do
  preparePosgreSqlConnection <- L.runIO $ EnvVars.getPreparePosgreSqlConnection
  when preparePosgreSqlConnection (preparePsqlMasterConnection esqDBCfg)
  preparePosgreSqlR1Connection <- L.runIO $ EnvVars.getPreparePosgreSqlR1Connection
  when preparePosgreSqlR1Connection (preparePsqlR1Connection esqDBReplicaCfg)

--   when EnvVars.getPrepareRedisClusterConnection prepareRedisClusterConnection

preparePsqlMasterConnection :: L.MonadFlow m => KSEC.EsqDBConfig -> m ()
preparePsqlMasterConnection conf = do
  pgConnName <- L.runIO $ EnvVars.postgresConnectionName
  preparePSqlConnection pgConnName KBT.PsqlDbCfg conf

preparePsqlR1Connection :: L.MonadFlow m => KSEC.EsqDBConfig -> m ()
preparePsqlR1Connection conf = do
  pgConnName <- L.runIO $ EnvVars.postgresR1ConnectionName
  preparePSqlConnection pgConnName KBT.PsqlDbCfgR1 conf

preparePSqlConnection :: L.MonadFlow m => (ET.OptionEntity a (ET.DBConfig BP.Pg)) => Text -> a -> KSEC.EsqDBConfig -> m ()
preparePSqlConnection pgConnName psqlDBCfgId KSEC.EsqDBConfig {..} = do
  poolConf <- L.runIO postgresPoolConfig
  let psqlDbCfg = ET.mkPostgresPoolConfig pgConnName pgConf poolConf
  ePoolPsql <- L.initSqlDBConnection psqlDbCfg
  L.setOption psqlDBCfgId psqlDbCfg
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
  stripes <- EnvVars.getPostgresPoolStripes
  keepAlive <- fromInteger <$> EnvVars.getPostgresPoolIdleTime
  resourcesPerStripe <- EnvVars.getPostgresPoolMax
  pure $ ET.PoolConfig {..}
