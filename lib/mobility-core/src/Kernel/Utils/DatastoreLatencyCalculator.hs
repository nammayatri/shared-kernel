{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.DatastoreLatencyCalculator where

import Kernel.Prelude
import Kernel.Storage.Hedis.Config
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Utils.Logging
import Kernel.Utils.Time

withTimeRedis ::
  ( MonadReader r m,
    HedisFlow m r,
    Log m,
    Monad m,
    MonadClock m,
    MonadTime m,
    CoreMetrics m
  ) =>
  Text ->
  Text ->
  m a ->
  m a
withTimeRedis storeType operationName operation = do
  enableRedisLatencyLogging <- asks (.enableRedisLatencyLogging)
  enablePrometheusMetricLogging <- asks (.enablePrometheusMetricLogging)
  withTime storeType operationName enableRedisLatencyLogging enablePrometheusMetricLogging operation

withTime ::
  ( MonadReader r m,
    Log m,
    Monad m,
    MonadClock m,
    MonadTime m,
    CoreMetrics m
  ) =>
  Text ->
  Text ->
  Bool ->
  Bool ->
  m a ->
  m a
withTime storeType operationName enableKibanaLatencyLogging enablePrometheusMetricLogging operation = do
  (res, latency) <- measureDuration operation
  when enableKibanaLatencyLogging $ logTagInfo (storeType <> ":" <> operationName) $ show latency
  when enablePrometheusMetricLogging $ addDatastoreLatency storeType operationName latency
  pure res

withTimeGeneric ::
  ( MonadReader r m,
    Log m,
    Monad m,
    MonadClock m,
    MonadTime m,
    CoreMetrics m
  ) =>
  Text ->
  m a ->
  m (a, Milliseconds)
withTimeGeneric operationName operation = do
  (res, latency) <- measureDuration operation
  addGenericLatency operationName latency
  pure (res, latency)

withTimeAPI ::
  ( MonadReader r m,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    Log m,
    Monad m,
    MonadClock m,
    MonadTime m,
    CoreMetrics m
  ) =>
  Text ->
  Text ->
  m a ->
  m a
withTimeAPI storeType operationName operation = do
  enableAPILatencyLogging <- asks (.enableAPILatencyLogging)
  enableAPIPrometheusMetricLogging <- asks (.enableAPIPrometheusMetricLogging)
  withTime storeType operationName enableAPILatencyLogging enableAPIPrometheusMetricLogging operation
