module Beckn.Storage.Esqueleto.Config where

import Beckn.Storage.Esqueleto.Logger (runLoggerIO)
import Beckn.Types.App (MonadFlow)
import Beckn.Types.Time (MonadTime)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Data.Pool (Pool)
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple (execute_)
import Database.PostgreSQL.Simple.Types (Query (Query))
import EulerHS.Prelude
import GHC.Records.Extra

data EsqDBConfig = EsqDBConfig
  { connectHost :: Text,
    connectPort :: Word16,
    connectUser :: Text,
    connectPassword :: Text,
    connectDatabase :: Text,
    connectSchemaName :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, FromDhall)

newtype EsqDBEnv = EsqDBEnv
  { connPool :: Pool SqlBackend
  }
  deriving (Generic)

prepareEsqDBEnv :: EsqDBConfig -> LoggerEnv -> IO EsqDBEnv
prepareEsqDBEnv cfg logEnv = do
  let connStr = makeConnString cfg
      modifyConnString = encodeUtf8 cfg.connectSchemaName
  let checkedLogEnv =
        if logEnv.logRawSql
          then logEnv
          else logEnv {fileLogger = Nothing, consoleLogger = Nothing}
  pool <- liftIO . runLoggerIO checkedLogEnv $ createPostgresqlPoolModified (modifyConn modifyConnString) connStr 10
  return $ EsqDBEnv pool
  where
    makeConnString dbConfig =
      encodeUtf8 $
        "host=" <> dbConfig.connectHost
          <> " dbname="
          <> dbConfig.connectDatabase
          <> " user="
          <> dbConfig.connectUser
          <> " password="
          <> dbConfig.connectPassword
          <> " port="
          <> show dbConfig.connectPort
    modifyConn schemaName conn =
      void . execute_ conn . Query $ "set search_path to " <> schemaName <> ", public; "

type HasEsq m r = (MonadReader r m, HasLog r, MonadTime m, MonadIO m)

type HasEsqEnv m r = (HasEsq m r, HasField "esqDBEnv" r EsqDBEnv)

type HasEsqReplica m r = (HasEsq m r, HasField "esqDBReplicaEnv" r EsqDBEnv)

type EsqDBFlow m r = (HasEsqEnv m r, MonadFlow m)

type EsqDBReplicaFlow m r = (HasEsqReplica m r, MonadFlow m)
