{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.Esqueleto.Config where

import Data.Pool (Pool)
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple (execute_)
import Database.PostgreSQL.Simple.Types (Query (Query))
import EulerHS.Prelude
import GHC.Records.Extra
import Kernel.Storage.Esqueleto.Logger (runLoggerIO)
import Kernel.Types.App (MonadFlow)
import Kernel.Types.Time (MonadTime)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging

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

type EsqDBFlow m r = (HasEsqEnv m r, MonadFlow m, Typeable m)

type EsqDBReplicaFlow m r = (HasEsqReplica m r, MonadFlow m, Typeable m)
