 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.Esqueleto.Migration
  ( migrateIfNeeded,
    migrateIfNeeded',
  )
where

import Data.ByteString
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.Migration
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Common
import Kernel.Utils.Common

fromEsqDBConfig :: EsqDBConfig -> PS.ConnectInfo
fromEsqDBConfig EsqDBConfig {..} =
  PS.ConnectInfo
    { connectHost = T.unpack connectHost,
      connectPort,
      connectUser = T.unpack connectUser,
      connectPassword = T.unpack connectPassword,
      connectDatabase = T.unpack connectDatabase
    }

migrateIfNeeded :: (MonadMask m, MonadIO m, Log m) => Maybe FilePath -> Bool -> EsqDBConfig -> m (Either String ())
migrateIfNeeded mPath autoMigrate esqDbConfig =
  migrateIfNeeded' mPath autoMigrate schemaName connectInfo
  where
    schemaName = encodeUtf8 esqDbConfig.connectSchemaName
    connectInfo = fromEsqDBConfig esqDbConfig

migrateIfNeeded' :: (MonadMask m, MonadIO m, Log m) => Maybe FilePath -> Bool -> ByteString -> PS.ConnectInfo -> m (Either String ())
migrateIfNeeded' mPath autoMigrate schemaName connectInfo =
  case mPath of
    Just path
      | autoMigrate ->
        bracket
          (liftIO (PS.connect connectInfo))
          (liftIO . PS.close)
          (migrate path)
    _ ->
      pure $ Right ()
  where
    options =
      defaultOptions
        { optTableName = schemaName <> "." <> "schema_migrations",
          optVerbose = Verbose
        }
    resultToEither MigrationSuccess = Right ()
    resultToEither (MigrationError a) = Left a
    migrate path conn =
      fmap resultToEither $ do
        logInfo $ "Running migrations (" <> show path <> ") ..."
        liftIO $
          runMigrations
            conn
            options
            [ MigrationInitialization,
              MigrationDirectory path
            ]
