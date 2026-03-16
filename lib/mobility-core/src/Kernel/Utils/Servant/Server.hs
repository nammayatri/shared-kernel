{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE UndecidableInstances #-}

module Kernel.Utils.Servant.Server where

import qualified Data.Aeson as A
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import qualified Database.Esqueleto.Experimental as Esq
import qualified Database.Persist.Sql as Persist
import qualified Database.Redis as Hedis
import EulerHS.Prelude
import qualified EulerHS.Runtime as E
import GHC.Records.Extra (HasField)
import Kernel.Prelude (identity)
import Kernel.Storage.Esqueleto.Config (EsqDBEnv (..))
import Kernel.Storage.Hedis.Config (HedisEnv (..))
import qualified Kernel.Tools.Metrics.CoreMetrics.Types as Metrics
import qualified Kernel.Tools.Metrics.Init as Metrics
import Kernel.Tools.Slack.Internal
import Kernel.Tools.Slack.Middleware
import Kernel.Types.App (EnvR (..), FlowHandlerR, FlowServerR)
import Kernel.Types.Flow
import Kernel.Types.Time
import Kernel.Utils.App
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.IOLogging
import Kernel.Utils.Logging
import qualified Kernel.Utils.Monitoring.Prometheus.Servant as Metrics
import Network.Wai.Handler.Warp
  ( Port,
    Settings,
    defaultSettings,
    runSettings,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setPort,
  )
import Servant
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFailFatal)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

class HasEnvEntry r (context :: [Type]) | context -> r where
  getEnvEntry :: Context context -> EnvR r

instance {-# OVERLAPPABLE #-} HasEnvEntry r xs => HasEnvEntry r (notIt ': xs) where
  getEnvEntry (_ :. xs) = getEnvEntry xs

instance {-# OVERLAPPING #-} HasEnvEntry r (EnvR r ': xs) where
  getEnvEntry (x :. _) = x

run ::
  forall a r ctx.
  ( HasContextEntry (ctx .++ '[ErrorFormatters]) ErrorFormatters,
    HasServer a (EnvR r ': ctx)
  ) =>
  Proxy (a :: Type) ->
  FlowServerR r a ->
  Context ctx ->
  EnvR r ->
  Application
run apis server ctx env =
  serveWithContext apis (env :. ctx) $
    hoistServerWithContext apis (Proxy @(EnvR r ': ctx)) f server
  where
    f :: FlowHandlerR r m -> Handler m
    f r = do
      eResult <- liftIO . try $ runReaderT r env
      case eResult of
        Left err ->
          print @String ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res

runGeneric ::
  forall api env m ctx.
  ( HasContextEntry (env ': (ctx .++ '[ErrorFormatters])) ErrorFormatters,
    HasServer api (env ': ctx),
    MonadReader env m
  ) =>
  Proxy (api :: Type) ->
  ServerT api m ->
  Context ctx ->
  env ->
  (forall b. env -> m b -> IO b) ->
  Application
runGeneric apis server ctx env runMonad =
  serveWithContext apis (env :. ctx) $
    hoistServerWithContext apis (Proxy @(env ': ctx)) f server
  where
    f :: m a -> Handler a
    f action = do
      eResult <- liftIO . try $ runMonad env action
      case eResult of
        Left err ->
          print @String ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res

runFlowRDelayedIO :: EnvR r -> FlowR r b -> DelayedIO b
runFlowRDelayedIO env f =
  liftIO (try . runFlowR (flowRuntime env) (appEnv env) $ f)
    >>= either delayedFailFatal pure

runServer ::
  forall env (api :: Type) ctx.
  ( HasField "graceTerminationPeriod" env Seconds,
    HasField "isShuttingDown" env Shutdown,
    HasField "loggerConfig" env L.LoggerConfig,
    HasField "loggerEnv" env LoggerEnv,
    HasField "port" env Port,
    HasField "version" env Metrics.DeploymentVersion,
    HasField "requestId" env (Maybe Text),
    HasField "sessionId" env (Maybe Text),
    Metrics.SanitizedUrl api,
    HasContextEntry (ctx .++ '[ErrorFormatters]) ErrorFormatters,
    HasServer api (EnvR env ': ctx)
  ) =>
  env ->
  Proxy api ->
  FlowServerR env api ->
  (Application -> Application) ->
  (Settings -> Settings) ->
  Context ctx ->
  (E.FlowRuntime -> IO () -> IO ()) ->
  (env -> IO ()) ->
  (E.FlowRuntime -> FlowR env E.FlowRuntime) ->
  IO ()
runServer appEnv serverAPI serverHandler waiMiddleware waiSettings servantCtx serverStartAction shutdownAction initialize = do
  let port = appEnv.port
  hostname <- getPodName
  let loggerRt = L.getEulerLoggerRuntime hostname $ appEnv.loggerConfig
  let settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ getSeconds appEnv.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (shutdownAction appEnv))
          & setPort port
          & waiSettings
  let server = withModifiedEnv $ \modifiedEnv ->
        run serverAPI serverHandler servantCtx modifiedEnv
          & logRequestAndResponse modifiedEnv
          & Metrics.addServantInfo appEnv.version serverAPI
          & waiMiddleware
  E.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <-
      runFlowR flowRt appEnv $
        initialize flowRt <* logInfo ("Runtime created. Starting server at port " <> show port)
    serverStartAction flowRt' $ runSettings settings $ server (EnvR flowRt' appEnv)

runServerGeneric ::
  forall m env (api :: Type) ctx.
  ( MonadReader env m,
    HasField "graceTerminationPeriod" env Seconds,
    HasField "isShuttingDown" env Shutdown,
    HasField "loggerConfig" env L.LoggerConfig,
    HasField "loggerEnv" env LoggerEnv,
    HasField "requestId" env (Maybe Text),
    HasField "sessionId" env (Maybe Text),
    HasField "port" env Port,
    HasField "version" env Metrics.DeploymentVersion,
    Metrics.SanitizedUrl api,
    HasContextEntry (env ': (ctx .++ '[ErrorFormatters])) ErrorFormatters,
    HasServer api (env ': ctx)
  ) =>
  env ->
  Proxy api ->
  ServerT api m ->
  (Application -> Application) ->
  (Settings -> Settings) ->
  Context ctx ->
  (env -> IO () -> IO ()) ->
  (env -> IO ()) ->
  (forall q. env -> m q -> IO q) ->
  IO ()
runServerGeneric appEnv serverAPI serverHandler waiMiddleware waiSettings servantCtx serverStartAction shutdownAction runMonad = do
  let port = appEnv.port
  let settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ getSeconds appEnv.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (shutdownAction appEnv))
          & setPort port
          & waiSettings
  let server = withModifiedEnvGeneric $ \modifiedEnv ->
        let loggerFunc = \tag info -> logOutputIO (appendLogTag tag $ modifiedEnv.loggerEnv) INFO info modifiedEnv.requestId modifiedEnv.sessionId
         in runGeneric serverAPI serverHandler servantCtx modifiedEnv runMonad
              & logRequestAndResponseGeneric loggerFunc
              & Metrics.addServantInfo appEnv.version serverAPI
              & waiMiddleware
  serverStartAction appEnv $ runSettings settings $ server appEnv

-- | Process start time, captured once for uptime calculation.
processStartTimeRef :: IORef UTCTime
processStartTimeRef = unsafePerformIO (getCurrentTime >>= newIORef)
{-# NOINLINE processStartTimeRef #-}

data HealthStatus = HealthStatus
  { status :: Text,
    db :: Text,
    redis :: Text,
    uptime :: Text
  }
  deriving (Generic, Show)

instance A.ToJSON HealthStatus

instance A.FromJSON HealthStatus

type HealthCheckAPI = Get '[JSON] HealthStatus

-- | Timeout for each dependency check (2 seconds).
healthCheckTimeoutMicros :: Int
healthCheckTimeoutMicros = 2000000

healthCheck ::
  (HasField "esqDBEnv" env EsqDBEnv, HasField "hedisClusterEnv" env HedisEnv) =>
  ServerT HealthCheckAPI (FlowHandlerR env)
healthCheck = do
  env <- asks (.appEnv)
  startTime <- liftIO $ readIORef processStartTimeRef
  now <- liftIO getCurrentTime
  let uptimeText = formatUptime (diffUTCTime now startTime)
  (pgResult :: Maybe (Either SomeException ())) <-
    liftIO $
      timeout healthCheckTimeoutMicros $
        try $
          void $ Esq.runSqlPool (Persist.rawSql @(Persist.Single Int) "SELECT 1" []) env.esqDBEnv.connPool
  (redisResult :: Maybe (Either SomeException ())) <-
    liftIO $
      timeout healthCheckTimeoutMicros $
        try $ do
          res <- Hedis.runRedis env.hedisClusterEnv.hedisConnection Hedis.ping
          case res of
            Right Hedis.Pong -> pure ()
            _ -> throwM err503 {errBody = "Redis ping failed"}
  let dbStatus = case pgResult of
        Just (Right _) -> "ok"
        Just (Left _) -> "error"
        Nothing -> "timeout"
      redisStatus = case redisResult of
        Just (Right _) -> "ok"
        Just (Left _) -> "error"
        Nothing -> "timeout"
      isHealthy = dbStatus == "ok" && redisStatus == "ok"
      healthStatus =
        HealthStatus
          { status = if isHealthy then "healthy" else "unhealthy",
            db = dbStatus,
            redis = redisStatus,
            uptime = uptimeText
          }
  if isHealthy
    then pure healthStatus
    else
      throwM
        err503
          { errBody = A.encode healthStatus,
            errHeaders = [("Content-Type", "application/json")]
          }

formatUptime :: NominalDiffTime -> Text
formatUptime diff =
  let totalSecs = floor diff :: Integer
      days = totalSecs `div` 86400
      hours = (totalSecs `mod` 86400) `div` 3600
      mins = (totalSecs `mod` 3600) `div` 60
      secs = totalSecs `mod` 60
   in show days <> "d " <> show hours <> "h " <> show mins <> "m " <> show secs <> "s"

runHealthCheckServerWithService ::
  forall env ctx.
  ( HasField "graceTerminationPeriod" env Seconds,
    HasField "isShuttingDown" env Shutdown,
    HasField "loggerConfig" env L.LoggerConfig,
    HasField "loggerEnv" env LoggerEnv,
    HasField "requestId" env (Maybe Text),
    HasField "sessionId" env (Maybe Text),
    HasField "port" env Port,
    HasField "version" env Metrics.DeploymentVersion,
    HasField "esqDBEnv" env EsqDBEnv,
    HasField "hedisClusterEnv" env HedisEnv,
    HasContextEntry (ctx .++ '[ErrorFormatters]) ErrorFormatters
  ) =>
  env ->
  (Application -> Application) ->
  (Settings -> Settings) ->
  Context ctx ->
  (E.FlowRuntime -> IO ()) ->
  (env -> IO ()) ->
  (E.FlowRuntime -> FlowR env E.FlowRuntime) ->
  IO ()
runHealthCheckServerWithService appEnv waiMiddleware waiSettings servantCtx service =
  runServer appEnv (Proxy @HealthCheckAPI) healthCheck waiMiddleware waiSettings servantCtx forkServerStartService
  where
    forkServerStartService flowRt startServerAction = do
      void $ forkIO startServerAction
      service flowRt

runServerWithHealthCheck ::
  forall env (api :: Type) ctx.
  ( HasField "graceTerminationPeriod" env Seconds,
    HasField "isShuttingDown" env Shutdown,
    HasField "loggerConfig" env L.LoggerConfig,
    HasField "loggerEnv" env LoggerEnv,
    HasField "port" env Port,
    HasField "requestId" env (Maybe Text),
    HasField "sessionId" env (Maybe Text),
    HasField "version" env Metrics.DeploymentVersion,
    HasField "esqDBEnv" env EsqDBEnv,
    HasField "hedisClusterEnv" env HedisEnv,
    Metrics.SanitizedUrl api,
    HasContextEntry (ctx .++ '[ErrorFormatters]) ErrorFormatters,
    HasServer api (EnvR env ': ctx)
  ) =>
  env ->
  Proxy api ->
  FlowServerR env api ->
  (Application -> Application) ->
  (Settings -> Settings) ->
  Context ctx ->
  (env -> IO ()) ->
  (E.FlowRuntime -> FlowR env E.FlowRuntime) ->
  IO ()
runServerWithHealthCheck appEnv _ serverHandler waiMiddleware waiSettings servantCtx =
  runServer appEnv (Proxy @(HealthCheckAPI :<|> api)) (healthCheck :<|> serverHandler) waiMiddleware waiSettings servantCtx (const identity)

runServerWithHealthCheckAndSlackNotification ::
  forall env (api :: Type) ctx.
  ( HasField "graceTerminationPeriod" env Seconds,
    HasField "isShuttingDown" env Shutdown,
    HasField "loggerConfig" env L.LoggerConfig,
    HasField "loggerEnv" env LoggerEnv,
    HasField "requestId" env (Maybe Text),
    HasSlackEnv env,
    HasField "sessionId" env (Maybe Text),
    HasField "port" env Port,
    HasField "version" env Metrics.DeploymentVersion,
    HasField "esqDBEnv" env EsqDBEnv,
    HasField "hedisClusterEnv" env HedisEnv,
    Metrics.SanitizedUrl api,
    HasContextEntry (ctx .++ '[ErrorFormatters]) ErrorFormatters,
    HasServer api (EnvR env ': ctx)
  ) =>
  env ->
  Proxy api ->
  FlowServerR env api ->
  (Application -> Application) ->
  (Settings -> Settings) ->
  Context ctx ->
  (env -> IO ()) ->
  (E.FlowRuntime -> FlowR env E.FlowRuntime) ->
  IO ()
runServerWithHealthCheckAndSlackNotification appEnv _ serverHandler waiMiddleware waiSettings servantCtx =
  runServer appEnv (Proxy @(HealthCheckAPI :<|> api)) (healthCheck :<|> serverHandler) moreMiddleware waiSettings servantCtx (const identity)
  where
    moreMiddleware app =
      waiMiddleware app
        & notifyOnSlackMiddleware appEnv
