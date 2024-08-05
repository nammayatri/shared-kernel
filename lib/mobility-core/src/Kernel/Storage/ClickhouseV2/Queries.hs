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
    RawQuery (..),
  )
where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V
import Database.ClickHouseDriver.HTTP
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2.ClickhouseDb
import Kernel.Storage.ClickhouseV2.ClickhouseTable
import Kernel.Storage.ClickhouseV2.Internal.ClickhouseColumns
import Kernel.Storage.ClickhouseV2.Internal.ClickhouseQuery
-- import Kernel.Storage.ClickhouseV2.Internal.Types
import Kernel.Utils.Common hiding (Limit, Offset)

-- should we throw error if query fails?
findAll ::
  forall a db t m cols gr ord.
  (HasClickhouseEnv db m, ClickhouseTable t, ClickhouseColumns a cols, ClickhouseQuery gr, ClickhouseQuery ord) =>
  Select a db t cols gr ord ->
  m [ColumnsType a cols]
findAll selectClause@(Select cols _ _) = do
  let rawQuery = toClickhouseQuery @(Select a db t cols gr ord) selectClause
  logDebug $ "clickhouse raw query v2: " <> T.pack rawQuery.getRawQuery
  resJSON <- runRawQuery @db @A.Value @m (Proxy @db) rawQuery
  case resJSON of
    Left err -> do
      logError $ "Clickhouse error: " <> T.pack err
      pure []
    Right val@(A.Array xs) -> do
      logDebug $ "clickhouse raw query v2 json result: " <> show val
      case mapM (parseColumns @a @cols (Proxy @a) cols) xs of
        Left err -> do
          logError $ "Clickhouse parsing result error: " <> T.pack err
          pure []
        Right ys -> pure (V.toList ys)
    Right val -> do
      logDebug $ "clickhouse raw query v2 json result: " <> show val
      logError "Expected Array"
      pure []

runRawQuery :: forall db a m. (HasClickhouseEnv db m, FromJSON a) => Proxy db -> RawQuery -> m (Either String a)
runRawQuery db query = runClickhouse @db @a @m db (`runQuery` getJSON query.getRawQuery)

runClickhouse :: forall db a m. (MonadFlow m, HasClickhouseEnv db m, FromJSON a) => Proxy db -> (HttpConnection -> IO (Either String a)) -> m (Either String a)
runClickhouse db action = do
  con <- getClickhouseEnv db
  L.runIO $ action con.connection
