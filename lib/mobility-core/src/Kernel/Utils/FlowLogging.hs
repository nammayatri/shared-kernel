{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.FlowLogging
  ( LoggerConfig (..),
    getEulerLoggerRuntime,
    appendLogContext,
    logOutputImplementation,
    withLogTagImplementation,
  )
where

import Data.Aeson as A
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Runtime
import EulerHS.Types (LogContext)
import qualified EulerHS.Types as T
import Kernel.Types.Logging
import System.Logger (DateFormat, Renderer, renderDefault)
import qualified Prelude as P

logOutputImplementation :: L.MonadFlow m => LogLevel -> T.Message -> m ()
logOutputImplementation logLevel message =
  case logLevel of
    DEBUG -> L.logDebug EmtpyTag message
    INFO -> L.logInfo EmtpyTag message
    WARNING -> L.logWarning EmtpyTag message
    ERROR -> L.logError EmtpyTag message

withLogTagImplementation ::
  L.MonadFlow m =>
  Text ->
  ReaderT r L.Flow a ->
  ReaderT r m a
withLogTagImplementation lc flowR =
  ReaderT $
    L.withLoggerContext (appendLogContext lc)
      . runReaderT flowR

data EmtpyTag = EmtpyTag

instance P.Show EmtpyTag where
  show _ = ""

formatTag :: Text -> Text
formatTag tag = "[" <> tag <> "]"

appendLogContext :: Text -> LogContext -> LogContext
appendLogContext val lc =
  let oldLCText = fromMaybe "" $ HM.lookup logContextKey lc
   in HM.insert logContextKey (oldLCText <> formatTag val) lc

getEulerLoggerConfig :: LoggerConfig -> T.LoggerConfig
getEulerLoggerConfig LoggerConfig {..} =
  T.defaultLoggerConfig
    { T._isAsync = True,
      T._logLevel = logLevel,
      T._logToFile = logToFile,
      T._logFilePath = eulLogsFilePath,
      T._logToConsole = logToConsole,
      T._logRawSql = logSql
    }
  where
    logLevel = case level of
      DEBUG -> T.Debug
      INFO -> T.Info
      WARNING -> T.Warning
      ERROR -> T.Error
    logSql =
      if logRawSql
        then T.UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION
        else T.SafelyOmitSqlLogs
    eulLogsFilePath = do
      let (l, r) = Text.breakOnEnd "." (Text.pack logFilePath)
      Text.unpack $
        if null l
          then r <> "-eul"
          else Text.init l <> "-eul." <> r

getEulerLoggerRuntime :: Maybe Text -> LoggerConfig -> IO LoggerRuntime
getEulerLoggerRuntime hostname = createOwnLoggerRuntime (logFlowFormatter hostname) . getEulerLoggerConfig

createOwnLoggerRuntime :: T.FlowFormatter -> T.LoggerConfig -> IO LoggerRuntime
createOwnLoggerRuntime = createLoggerRuntime' defaultDateFormat (Just ownRender) defaultBufferSize
  where
    ownRender :: Renderer
    ownRender s _ _ xs =
      let lbsFromBuilder = Builder.toLazyByteString $ renderDefault s xs
       in if LBS.length lbsFromBuilder > 3 -- 3 character, for deleting prefix
            then Builder.lazyByteString $ LBS.drop 3 lbsFromBuilder
            else Builder.lazyByteString lbsFromBuilder

    defaultBufferSize :: T.BufferSize
    defaultBufferSize = 4096 -- value from euler-hs
    defaultDateFormat :: Maybe DateFormat
    defaultDateFormat = Nothing -- value from euler-hs

logFlowFormatter :: Maybe Text -> T.FlowFormatter
logFlowFormatter hostname _ = do
  currTime <- Time.getCurrentTime
  pure $ logFormatterText currTime hostname

logFormatterText :: Time.UTCTime -> Maybe Text -> T.MessageFormatter
logFormatterText
  timestamp
  hostname
  (T.PendingMsg _mbFlowGuid elvl eTag msg msgNum logContHM) = res
    where
      logCont = HM.lookupDefault "" logContextKey logContHM
      tag = if null eTag || eTag == "\"\"" then "" else formatTag eTag
      lvl = case elvl of
        T.Debug -> DEBUG
        T.Warning -> WARNING
        T.Info -> INFO
        T.Error -> ERROR
      -- textToLBS = LBS.fromStrict . Txt.encodeUtf8
      log =
        show timestamp
          <> " "
          <> show lvl
          <> " "
          <> show msgNum
          <> "> @"
          <> fromMaybe "null" hostname
          <> " "
          <> logCont
          <> tag
          <> " |> "
          <> msg
      res =
        T.SimpleLBS (A.encode (A.Object $ HM.insert "log" (A.String log) HM.empty))

logContextKey :: Text
logContextKey = "log_context"
