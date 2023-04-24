module Kernel.Storage.Clickhouse.Config where

import qualified Control.Monad.Catch as C
import Data.String.Conversions
import Data.Text as T
import Data.Word (Word16)
import qualified Database.ClickHouseDriver.HTTP as CH
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Dhall (FromDhall)

type ClickhouseFlow m env =
  (MonadReader env m, HasField "clickhouseEnv" env ClickhouseEnv, MonadIO m, C.MonadThrow m, Log m)

data ClickhouseCfg = ClickhouseCfg
  { username :: Text,
    host :: Text,
    port :: Word16,
    password :: Text,
    database :: Text,
    tls :: Bool
  }
  deriving (Generic, FromDhall)

newtype ClickhouseEnv = ClickhouseEnv
  { connection :: CH.HttpConnection
  }

createConn :: ClickhouseCfg -> IO ClickhouseEnv
createConn ClickhouseCfg {..} =
  ClickhouseEnv
    <$> CH.httpConnectDb
      (if T.null database then Nothing else Just (cs database))
      (cs username)
      (cs password)
      (fromIntegral port)
      (cs host)
      tls
