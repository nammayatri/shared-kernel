module Kernel.Storage.Clickhouse.Config where

import qualified Control.Concurrent.MVar as M
import qualified Control.Monad.Catch as C
import Data.String.Conversions
import Data.Text as T
import Data.Time.Clock hiding (getCurrentTime)
import qualified Data.Vector as V
import Data.Word (Word16)
import qualified Database.ClickHouseDriver.HTTP as CH
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Dhall (FromDhall)

type ClickhouseFlow m env =
  ( MonadReader env m,
    HasField "kafkaClickhouseEnv" env ClickhouseEnv,
    HasField "serviceClickhouseEnv" env ClickhouseEnv,
    HasField "dashboardClickhouseEnv" env ClickhouseEnv,
    MonadIO m,
    C.MonadThrow m,
    Log m,
    HasField "serviceClickhouseCfg" env ClickhouseCfg,
    HasField "kafkaClickhouseCfg" env ClickhouseCfg,
    HasField "dashboardClickhouseCfg" env ClickhouseCfg
  )

data ClickhouseCfg = ClickhouseCfg
  { username :: Text,
    host :: Text,
    port :: Word16,
    password :: Text,
    database :: Text,
    tls :: Bool,
    retryInterval :: V.Vector Int
  }
  deriving (Generic, FromDhall)

data ClickhouseDb = APP_SERVICE_CLICKHOUSE | ATLAS_KAFKA

data ClickhouseData = ClickhouseData
  { connection :: CH.HttpConnection,
    retryInterval :: V.Vector Int,
    lastTryIndex :: Int,
    lastTryTime :: UTCTime,
    status :: Bool
  }

newtype ClickhouseEnv = ClickhouseEnv
  { connectionData :: M.MVar ClickhouseData
  }

createConn :: ClickhouseCfg -> IO ClickhouseEnv
createConn ClickhouseCfg {..} = do
  conn <-
    CH.httpConnectDb
      (if T.null database then Nothing else Just (cs database))
      (cs username)
      (cs password)
      (fromIntegral port)
      (cs host)
      tls
  time <- getCurrentTime
  ClickhouseEnv <$> M.newMVar (ClickhouseData {connection = conn, lastTryIndex = -1, lastTryTime = time, status = False, ..})

getLock :: ClickhouseEnv -> IO Bool
getLock env = do
  envData <- M.takeMVar env.connectionData
  let status = envData.status
  M.putMVar env.connectionData envData {status = True}
  return status

releaseLock :: ClickhouseEnv -> IO ()
releaseLock env = do
  envData <- M.readMVar env.connectionData
  M.modifyMVar_ env.connectionData (\_ -> pure $ envData {status = False})

connectionHelper :: ClickhouseCfg -> ClickhouseEnv -> IO ()
connectionHelper cfg env = do
  lock <- getLock env
  when (not lock) $ do
    envData <- M.readMVar env.connectionData
    when (V.length envData.retryInterval /= 0) do
      let retryIndex = min (envData.lastTryIndex + 1) ((V.length envData.retryInterval) -1)
      let retryTime = envData.retryInterval V.! retryIndex
      time <- liftIO getCurrentTime
      if diffUTCTime time (lastTryTime envData) > fromIntegral retryTime
        then do
          con <- liftIO $ createCkhConn cfg
          M.modifyMVar_ env.connectionData (\_ -> pure $ envData {connection = con, lastTryIndex = retryIndex, lastTryTime = time})
        else pure ()
    releaseLock env

retryClickhouseConnection :: (MonadFlow m, ClickhouseFlow m env) => ClickhouseDb -> m ()
retryClickhouseConnection db = do
  case db of
    APP_SERVICE_CLICKHOUSE -> do
      clickhouseEnv <- asks (.serviceClickhouseEnv)
      ckhCfg <- asks (.serviceClickhouseCfg)
      liftIO $ connectionHelper ckhCfg clickhouseEnv
    ATLAS_KAFKA -> do
      clickhouseEnv <- asks (.kafkaClickhouseEnv)
      ckhCfg <- asks (.kafkaClickhouseCfg)
      liftIO $ connectionHelper ckhCfg clickhouseEnv

createCkhConn :: ClickhouseCfg -> IO CH.HttpConnection
createCkhConn ClickhouseCfg {..} =
  CH.httpConnectDb
    (if T.null database then Nothing else Just (cs database))
    (cs username)
    (cs password)
    (fromIntegral port)
    (cs host)
    tls
