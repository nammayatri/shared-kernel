{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.Esqueleto.Migration
  ( migrateIfNeeded,
    migrateIfNeeded',
  )
where

import Data.ByteString hiding (map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.Migration
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Common
import Kernel.Utils.Common
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, listDirectory, removePathForcibly)
import System.FilePath (takeExtension, (</>))

fromEsqDBConfig :: EsqDBConfig -> PS.ConnectInfo
fromEsqDBConfig EsqDBConfig {..} =
  PS.ConnectInfo
    { connectHost = T.unpack connectHost,
      connectPort,
      connectUser = T.unpack connectUser,
      connectPassword = T.unpack connectPassword,
      connectDatabase = T.unpack connectDatabase
    }

migrateIfNeeded :: (MonadMask m, MonadIO m, Log m) => [FilePath] -> Bool -> EsqDBConfig -> m (Either String ())
migrateIfNeeded mPaths autoMigrate esqDbConfig =
  migrateIfNeeded' mPaths autoMigrate schemaName connectInfo
  where
    schemaName = encodeUtf8 esqDbConfig.connectSchemaName
    connectInfo = fromEsqDBConfig esqDbConfig

migrateIfNeeded' :: (MonadMask m, MonadIO m, Log m) => [FilePath] -> Bool -> ByteString -> PS.ConnectInfo -> m (Either String ())
migrateIfNeeded' mPaths autoMigrate schemaName connectInfo =
  if autoMigrate
    then
      bracket
        (liftIO (PS.connect connectInfo))
        (liftIO . PS.close)
        (migrate mPaths)
    else pure $ Right ()
  where
    options =
      defaultOptions
        { optTableName = schemaName <> "." <> "schema_migrations",
          optVerbose = Verbose
        }
    resultToEither MigrationSuccess = Right ()
    resultToEither (MigrationError a) = Left a
    migrate paths conn = do
      patchedPaths <- liftIO $ stripConcurrentlyForLocal paths
      fmap resultToEither $ do
        logInfo $ "Running migrations (" <> show patchedPaths <> ") ..."
        liftIO $
          runMigrations
            conn
            options
            ([MigrationInitialization] <> map MigrationDirectory patchedPaths)

-- | `CREATE INDEX CONCURRENTLY` cannot run inside a transaction (SQLSTATE 25001),
-- but `postgresql-simple-migration` wraps each .sql in one. `autoMigrate = True`
-- is local-only; prod applies migrations via `psql -f` where CONCURRENTLY works.
-- Stage rewritten copies in a temp dir; source files stay untouched.
stripConcurrentlyForLocal :: [FilePath] -> IO [FilePath]
stripConcurrentlyForLocal paths = do
  tmp <- getTemporaryDirectory
  let root = tmp </> "ny-migrations"
  removePathForcibly root
  zipWithM (stage root) [0 :: Int ..] paths
  where
    stage root idx src = do
      let dst = root </> show idx
      createDirectoryIfMissing True dst
      entries <- listDirectory src
      forM_ entries $ \name -> when (takeExtension name == ".sql") $ do
        body <- TIO.readFile (src </> name)
        TIO.writeFile (dst </> name) (rewrite body)
      pure dst
    rewrite =
      T.replace "CREATE INDEX CONCURRENTLY" "CREATE INDEX"
        . T.replace "CREATE UNIQUE INDEX CONCURRENTLY" "CREATE UNIQUE INDEX"
