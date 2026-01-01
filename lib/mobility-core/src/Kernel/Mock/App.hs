{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Kernel.Mock.App where

import qualified Control.Concurrent.MVar as M
import qualified Control.Monad.Catch as C
import Control.Monad.IO.Unlift
import qualified EulerHS.Types as ET
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Utils.IOLogging
import Servant
import Universum
import UnliftIO.Concurrent

type HealthCheckAPI = Get '[JSON] Text

healthCheckServer :: MockM e Text
healthCheckServer = do
  pure "Mock is up!"

run :: forall e api. HasServer api '[] => Proxy (api :: Type) -> ServerT api (MockM e) -> e -> Application
run _ server env = serve proxyApi $ hoistServer proxyApi f (healthCheckServer :<|> server)
  where
    proxyApi = Proxy @(HealthCheckAPI :<|> api)
    f :: MockM e a -> Handler a
    f action = do
      eithRes <- liftIO . C.try $ runReaderT (runMockM action) env
      case eithRes of
        Left err ->
          liftIO $ print @String ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res

newtype MockM e a = MockM {runMockM :: ReaderT e IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader e, MonadIO, MonadUnliftIO, C.MonadThrow, C.MonadCatch, C.MonadMask)

runMock :: e -> MockM e a -> IO a
runMock env action = runReaderT (runMockM action) env

instance CoreMetrics (MockM e) where
  addRequestLatency _ _ _ _ = return ()
  addDatastoreLatency _ _ _ = return ()
  incrementErrorCounter _ _ = return ()
  addUrlCallRetries _ _ = return ()
  addUrlCallRetryFailures _ = return ()
  incrementSortedSetCounter _ = return ()
  incrementStreamCounter _ = return ()
  incrementStreamFailedCounter _ = return ()
  addGenericLatency _ _ = return ()
  incrementSchedulerFailureCounter _ = return ()
  incrementProducerError _ = return ()
  incrementGenericMetrics _ = return ()
  incrementSystemConfigsFailedCounter _ = return ()
  addGenericLatencyMetrics _ _ = return ()
  addOpenTripPlannerResponse _ _ _ = return ()
  addOpenTripPlannerLatency _ _ _ = return ()
  incrementTryExceptionCounter _ _ = return ()

instance MonadTime (MockM e) where
  getCurrentTime = liftIO getCurrentTime

instance MonadClock (MockM e) where
  getClockTime = liftIO getClockTime

instance (HasLog e, HasField "requestId" e (Maybe Text), HasField "sessionId" e (Maybe Text)) => Log (MockM e) where
  logOutput = logOutputImplementation
  withLogTag = withLogTagImplementation

instance (HasLog e) => Forkable (MockM e) where
  fork = mockFork
  forkMultiple tagAndFunction = forM_ tagAndFunction $ \(tag, f) -> mockFork tag f -- it works with multiple threads unlike forkMultiple @(FlowR r), which creates only one thread
  awaitableFork = mockAwaitableFork

instance (HasLog e) => TryException (MockM e) where
  withTryCatch = mockWithTryCatch

instance MonadGuid (MockM e) where
  generateGUIDText = liftIO generateGUIDTextIO

mockFork :: (HasLog e) => Text -> MockM e a -> MockM e ()
mockFork tag action = void $
  withLogTag tag $
    forkFinally action $ \case
      Left se -> logOutput ERROR $ show se
      Right _ -> pure ()

mockAwaitableFork :: (HasLog e) => Text -> MockM e a -> MockM e (ET.Awaitable (Either Text a))
mockAwaitableFork tag action = do
  awaitableMVar <- liftIO M.newEmptyMVar
  void . withLogTag tag $
    forkFinally action $ \case
      Left se -> do
        logOutput ERROR $ show se
        liftIO $ M.putMVar awaitableMVar $ Left $ show se
      Right res -> do
        liftIO $ M.putMVar awaitableMVar $ Right res
  pure $ ET.Awaitable awaitableMVar

mockWithTryCatch :: (HasLog e) => Text -> MockM e a -> MockM e (Either SomeException a)
mockWithTryCatch _ action = do
  res <- try action
  case res of
    Right a -> pure $ Right a
    Left e -> do
      logOutput ERROR $ show e
      pure $ Left e
