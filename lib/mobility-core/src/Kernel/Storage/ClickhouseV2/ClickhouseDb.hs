{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.ClickhouseV2.ClickhouseDb (module Kernel.Storage.ClickhouseV2.ClickhouseDb, module Reexport) where

-- import Data.String.Conversions
-- import Data.Text as T
-- import Data.Word (Word16)
-- import qualified Database.ClickHouseDriver.HTTP as CH
import Kernel.Prelude
-- import Kernel.Utils.Dhall (FromDhall)
import Kernel.Storage.Clickhouse.Config as Reexport (ClickhouseCfg (..), ClickhouseEnv (..), createConn)
import Kernel.Types.Common

class ClickhouseDb db

class (ClickhouseDb db, MonadFlow m) => HasClickhouseEnv db m where
  getClickhouseEnv :: Proxy db -> m ClickhouseEnv
  getClickhouseCfg :: Proxy db -> m ClickhouseCfg

data APP_SERVICE_CLICKHOUSE

instance ClickhouseDb APP_SERVICE_CLICKHOUSE

instance (MonadFlow m, MonadReader r m, HasField "serviceClickhouseEnv" r ClickhouseEnv, HasField "serviceClickhouseCfg" r ClickhouseCfg) => HasClickhouseEnv APP_SERVICE_CLICKHOUSE m where
  getClickhouseEnv _ = asks (.serviceClickhouseEnv)
  getClickhouseCfg _ = asks (.serviceClickhouseCfg)

data ATLAS_KAFKA

instance ClickhouseDb ATLAS_KAFKA

instance (MonadFlow m, MonadReader r m, HasField "kafkaClickhouseEnv" r ClickhouseEnv, HasField "kafkaClickhouseCfg" r ClickhouseCfg) => HasClickhouseEnv ATLAS_KAFKA m where
  getClickhouseEnv _ = asks (.kafkaClickhouseEnv)
  getClickhouseCfg _ = asks (.kafkaClickhouseCfg)

-- data ClickhouseCfg = ClickhouseCfg
--   { username :: Text,
--     host :: Text,
--     port :: Word16,
--     password :: Text,
--     database :: Text,
--     tls :: Bool
--   }
--   deriving (Generic, FromDhall)

-- newtype ClickhouseEnv = ClickhouseEnv
--   { connection :: CH.HttpConnection
--   }

-- createConn :: ClickhouseCfg -> IO ClickhouseEnv
-- createConn ClickhouseCfg {..} =
--   ClickhouseEnv
--     <$> CH.httpConnectDb
--       (if T.null database then Nothing else Just (cs database))
--       (cs username)
--       (cs password)
--       (fromIntegral port)
--       (cs host)
--       tls
