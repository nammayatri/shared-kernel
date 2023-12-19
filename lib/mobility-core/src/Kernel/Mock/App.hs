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
{-# OPTIONS_GHC -Wwarn=missing-methods #-}

module Kernel.Mock.App where

import qualified Control.Monad.Catch as C
import Control.Monad.IO.Unlift
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
  addGenericLatency _ _ = return ()
  incrementSchedulerFailureCounter _ = return ()
  incrementGenericMetrics _ = return ()
  incrementKvConfigFailedCounter _ = return ()

instance MonadTime (MockM e) where
  getCurrentTime = liftIO getCurrentTime

instance MonadClock (MockM e) where
  getClockTime = liftIO getClockTime

instance (HasLog e) => Log (MockM e) where
  logOutput = logOutputImplementation
  withLogTag = withLogTagImplementation

instance (HasLog e) => Forkable (MockM e) where
  fork = mockFork

instance MonadGuid (MockM e) where
  generateGUIDText = liftIO generateGUIDTextIO

mockFork :: (HasLog e) => Text -> MockM e a -> MockM e ()
mockFork tag action = void $
  withLogTag tag $
    forkFinally action $ \case
      Left se -> logOutput ERROR $ show se
      Right _ -> pure ()
