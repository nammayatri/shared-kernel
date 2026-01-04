{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.IOLogging
  ( LoggerConfig (..),
    Logger,
    LoggerEnv (..),
    HasLog,
    prepareLoggerEnv,
    releaseLoggerEnv,
    logOutputImplementation,
    withLogTagImplementation,
    logOutputIO,
    appendLogTag,
    withLoggerEnv,
    updateLogLevelAndRawSql,
  )
where

import qualified Control.Monad.Catch as C
import Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import Data.List (partition)
import qualified Data.Text as T
import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Types.Logging
import Kernel.Types.Time
import System.Log.FastLogger

type HasLog r = (HasField "loggerEnv" r LoggerEnv, HasField "requestId" r (Maybe Text), HasField "sessionId" r (Maybe Text))

data Logger = Logger
  { printLogFunc :: FastLogger,
    cleanUpFunc :: IO ()
  }

data LoggerEnv = LoggerEnv
  { level :: LogLevel,
    hostName :: Maybe Text,
    tags :: [Text],
    fileLogger :: Maybe Logger,
    consoleLogger :: Maybe Logger,
    logRawSql :: Bool
  }

withLoggerEnv :: LoggerConfig -> Maybe Text -> (LoggerEnv -> IO a) -> IO a
withLoggerEnv loggerConfig hostName = C.bracket (prepareLoggerEnv loggerConfig hostName) releaseLoggerEnv

prepareLoggerEnv :: LoggerConfig -> Maybe Text -> IO LoggerEnv
prepareLoggerEnv loggerConfig hostName = do
  fileLogger <-
    if loggerConfig.logToFile
      then Just <$> prepareLogger (LogFileNoRotate loggerConfig.logFilePath defaultBufSize)
      else return Nothing

  consoleLogger <-
    if loggerConfig.logToConsole
      then Just <$> prepareLogger (LogStdout defaultBufSize)
      else return Nothing

  return $
    LoggerEnv
      { level = loggerConfig.level,
        logRawSql = loggerConfig.logRawSql,
        tags = [],
        ..
      }
  where
    prepareLogger logType = do
      (printLogFunc, cleanUpFunc) <- newFastLogger logType
      return $ Logger {..}

releaseLoggerEnv :: LoggerEnv -> IO ()
releaseLoggerEnv LoggerEnv {..} = do
  whenJust fileLogger $ \logger -> logger.cleanUpFunc
  whenJust consoleLogger $ \logger -> logger.cleanUpFunc

logOutputImplementation :: (HasLog r, MonadReader r m, MonadIO m, MonadTime m, HasField "requestId" r (Maybe Text), HasField "sessionId" r (Maybe Text)) => LogLevel -> Text -> m ()
logOutputImplementation logLevel message = do
  logEnv <- asks (.loggerEnv)
  requestId <- asks (.requestId)
  sessionId <- asks (.sessionId)
  logOutputIO logEnv logLevel message requestId sessionId

logOutputIO :: (MonadIO m, MonadTime m) => LoggerEnv -> LogLevel -> Text -> Maybe Text -> Maybe Text -> m ()
logOutputIO logEnv logLevel message requestId sessionId = do
  when (logLevel >= logEnv.level) $ do
    now <- getCurrentTime
    let formattedMessage = logFormatterText now logEnv.hostName logLevel logEnv.tags message requestId sessionId
    whenJust logEnv.fileLogger $ \logger ->
      liftIO . logger.printLogFunc $ toLogStr (A.encode formattedMessage <> "\n")
    whenJust logEnv.consoleLogger $ \logger ->
      liftIO . logger.printLogFunc $ toLogStr (A.encode formattedMessage <> "\n")

withLogTagImplementation ::
  (HasLog r, MonadReader r m) =>
  Text ->
  m a ->
  m a
withLogTagImplementation tag = local modifyEnv
  where
    modifyEnv env = do
      let logEnv = env.loggerEnv
          updLogEnv = appendLogTag tag logEnv
      env{loggerEnv = updLogEnv}

appendLogTag :: Text -> LoggerEnv -> LoggerEnv
appendLogTag tag logEnv = do
  logEnv{tags = tag : logEnv.tags}

updateLogLevelAndRawSql :: Maybe LogLevel -> LoggerEnv -> LoggerEnv
updateLogLevelAndRawSql mbNewLogLevel logEnv =
  maybe
    logEnv
    (\newLogLevel -> logEnv{level = newLogLevel, logRawSql = newLogLevel == DEBUG})
    mbNewLogLevel

formatTags :: [Text] -> Maybe Text -> Text
formatTags tags mUrl =
  "[" <> T.intercalate ", " (reverse tags)
    <> maybe "" (\url -> ", url-" <> url) mUrl
    <> "]"

logFormatterText :: Time.UTCTime -> Maybe Text -> LogLevel -> [Text] -> Text -> Maybe Text -> Maybe Text -> A.Value
logFormatterText timestamp hostname lvl tags msg requestId sessionId = res
  where
    (urlTags, otherTags) = partition (T.isPrefixOf "url:") tags
    mUrl = listToMaybe urlTags <&> T.drop 4
    tag = if null otherTags then "" else formatTags otherTags mUrl
    log =
      show timestamp
        <> " "
        <> show lvl
        <> "> "
        <> maybe "" ("@" <>) hostname
        <> " "
        <> tag
        <> " |> "
        <> msg
    res =
      A.Object
        . AKM.insert "timestamp" (A.String $ show timestamp)
        . maybe (\a -> a) (AKM.insert "requestId" . A.String) requestId
        . maybe (\a -> a) (AKM.insert "sessionId" . A.String) sessionId
        . maybe (\a -> a) (AKM.insert "url" . A.String) mUrl
        $ AKM.insert "log" (A.String log) AKM.empty
