{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.Hedis.Config where

import qualified Control.Monad.Catch as C
import Data.Word (Word16)
import Database.Redis
import GHC.Records.Extra
import Kernel.Prelude
import Kernel.Types.Logging
import Kernel.Utils.Dhall (FromDhall)
import Network.Socket (HostName)

type HedisFlow m env =
  (MonadReader env m, HasField "hedisMigrationStage" env Bool, HasField "hedisClusterEnv" env HedisEnv, HasField "hedisEnv" env HedisEnv, MonadIO m, C.MonadThrow m, Log m)

type KeyModifierFunc = (Text -> Text)

data HedisCfg = HedisCfg
  { connectHost :: HostName,
    connectPort :: Word16,
    connectAuth :: Maybe Text,
    connectDatabase :: Integer,
    connectMaxConnections :: Int,
    connectMaxIdleTime :: NominalDiffTime,
    connectTimeout :: Maybe NominalDiffTime
  }
  deriving (Generic, Show, FromDhall)

data HedisEnv = HedisEnv
  { hedisConnection :: Connection,
    keyModifier :: KeyModifierFunc
  }
  deriving (Generic)

defaultHedisCfg :: HedisCfg
defaultHedisCfg =
  HedisCfg
    { connectHost = "localhost",
      connectPort = 6379,
      connectAuth = Nothing,
      connectDatabase = 0,
      connectMaxConnections = 50,
      connectMaxIdleTime = 30,
      connectTimeout = Nothing
    }

withHedisEnv :: HedisCfg -> KeyModifierFunc -> (HedisEnv -> IO a) -> IO a
withHedisEnv cfg keyModifier = C.bracket (connectHedis cfg keyModifier) disconnectHedis

connectHedisCluster :: HedisCfg -> KeyModifierFunc -> IO HedisEnv
connectHedisCluster cfg keyModifier = do
  conn <- connectCluster connectInfo
  return $
    HedisEnv
      { hedisConnection = conn,
        keyModifier = keyModifier
      }
  where
    connectInfo :: ConnectInfo
    connectInfo =
      defaultConnectInfo
        { connectHost = cfg.connectHost,
          connectPort = PortNumber $ toEnum $ fromEnum cfg.connectPort,
          connectAuth = encodeUtf8 <$> cfg.connectAuth,
          connectDatabase = cfg.connectDatabase,
          connectMaxConnections = cfg.connectMaxConnections,
          connectMaxIdleTime = cfg.connectMaxIdleTime
        }

connectHedis :: HedisCfg -> KeyModifierFunc -> IO HedisEnv
connectHedis cfg keyModifier = do
  conn <- checkedConnect connectInfo
  return $
    HedisEnv
      { hedisConnection = conn,
        keyModifier = keyModifier
      }
  where
    connectInfo :: ConnectInfo
    connectInfo =
      defaultConnectInfo
        { connectHost = cfg.connectHost,
          connectPort = PortNumber $ toEnum $ fromEnum cfg.connectPort,
          connectAuth = encodeUtf8 <$> cfg.connectAuth,
          connectDatabase = cfg.connectDatabase,
          connectMaxConnections = cfg.connectMaxConnections,
          connectMaxIdleTime = cfg.connectMaxIdleTime
        }

disconnectHedis :: HedisEnv -> IO ()
disconnectHedis HedisEnv {..} =
  disconnect hedisConnection
