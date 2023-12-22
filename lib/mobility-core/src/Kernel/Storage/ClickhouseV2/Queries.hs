{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.ClickhouseV2.Queries
  ( findAll,
    -- findOne,
    runRawQuery,
    RawQuery (getRawQuery),
  )
where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Database.ClickHouseDriver.HTTP
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2.ClickhouseDb
import Kernel.Storage.ClickhouseV2.ClickhouseTable
import Kernel.Storage.ClickhouseV2.Internal.ClickhouseQuery
import Kernel.Storage.ClickhouseV2.Internal.Types
import Kernel.Utils.Common hiding (Limit, Offset)

-- should we throw error if query fails?
findAll :: forall db t m. (HasClickhouseEnv db m, ClickhouseTable t) => Select db t -> m [t Identity]
findAll selectClause = do
  let rawQuery = toClickhouseQuery @(Select db t) selectClause
  logDebug $ "clickhouse raw query v2: " <> T.pack rawQuery.getRawQuery
  -- res <- runRawQuery @db @[t Identity] @m (Proxy @db) rawQuery
  resJSON <- runRawQuery @db @A.Value @m (Proxy @db) rawQuery
  case resJSON of
    Left err -> do
      logError $ "Clickhouse error: " <> show err
      pure []
    Right val -> do
      logDebug $ "clickhouse raw query v2 json result: " <> show val
      case A.fromJSON @[t Identity] val of
        A.Error err -> do
          logError $ "Clickhouse parsing result error: " <> show err
          pure []
        A.Success ys -> pure ys

runRawQuery :: forall db a m. (HasClickhouseEnv db m, FromJSON a) => Proxy db -> RawQuery -> m (Either String a)
runRawQuery db query = runClickhouse @db @a @m db (`runQuery` getJSON query.getRawQuery)

runClickhouse :: forall db a m. (MonadFlow m, HasClickhouseEnv db m, FromJSON a) => Proxy db -> (HttpConnection -> IO (Either String a)) -> m (Either String a)
runClickhouse db action = do
  con <- getClickhouseEnv db
  L.runIO $ action con.connection
