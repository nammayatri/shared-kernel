{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kernel.Storage.Esqueleto.Logger (LoggerIO (..), runLoggerIO) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger as CMLogger
  ( LogLevel (..),
    MonadLogger (..),
    MonadLoggerIO (..),
    ToLogStr (toLogStr),
    fromLogStr,
  )
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import EulerHS.Prelude hiding (Key)
import Kernel.Types.Logging as BLogging (Log (..), LogLevel (..))
import Kernel.Types.MonadGuid
import Kernel.Types.Time (MonadTime (..))
import Kernel.Utils.IOLogging (LoggerEnv, appendLogTag, logOutputIO)

--TODO: Remove this when we remove EulerHS
newtype LoggerIO a = LoggerIO (ReaderT LoggerEnv IO a)
  deriving stock (Generic)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow, MonadCatch, MonadMask)

instance MonadTime LoggerIO where
  getCurrentTime = liftIO getCurrentTime

runLoggerIO :: LoggerEnv -> LoggerIO a -> IO a
runLoggerIO logEnv (LoggerIO rdr) = runReaderT rdr logEnv

logFunc :: ToLogStr msg => LoggerEnv -> BLogging.LogLevel -> msg -> IO ()
logFunc logEnv logLevel msg =
  logOutputIO logEnv logLevel . decodeUtf8 . fromLogStr $ toLogStr msg

logLevelCMtoB :: CMLogger.LogLevel -> BLogging.LogLevel
logLevelCMtoB cmLogLevel = case cmLogLevel of
  LevelError -> ERROR
  LevelWarn -> WARNING
  LevelDebug -> DEBUG
  _ -> INFO

instance MonadLogger LoggerIO where
  monadLoggerLog _ _ logLevel msg = LoggerIO $ do
    loggerEnv <- ask
    liftIO $ logFunc loggerEnv (logLevelCMtoB logLevel) msg

instance MonadLoggerIO LoggerIO where
  askLoggerIO =
    LoggerIO $
      (\logEnv _ _ logLvl msg -> logFunc logEnv (logLevelCMtoB logLvl) msg) <$> ask

instance Log LoggerIO where
  logOutput logLevel msg = LoggerIO $ do
    loggerEnv <- ask
    liftIO $ logFunc loggerEnv logLevel msg

  withLogTag tag (LoggerIO logger) = LoggerIO $ local modifyEnv logger
    where
      modifyEnv logEnv = appendLogTag tag logEnv

instance MonadGuid LoggerIO where
  generateGUIDText = liftIO (UUID.toText <$> UUID.nextRandom)
