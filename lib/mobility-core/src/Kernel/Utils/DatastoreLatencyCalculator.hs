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

module Kernel.Utils.DatastoreLatencyCalculator where

import Kernel.Prelude
import Kernel.Storage.Hedis.Config
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Utils.Common

withTime ::
  ( MonadReader r m,
    HedisFlow m r,
    Log m,
    Monad m,
    MonadTime m,
    CoreMetrics m
  ) =>
  Text ->
  Text ->
  m a ->
  m a
withTime storeType operationName operation = do
  btime <- getCurrentTime
  res <- operation
  atime <- getCurrentTime
  let latency = diffUTCTime atime btime
  enableRedisLatencyLogging <- asks (.enableRedisLatencyLogging)
  when enableRedisLatencyLogging $ logTagInfo (storeType <> ":" <> operationName) $ show latency
  enablePrometheusMetricLogging <- asks (.enablePrometheusMetricLogging)
  when enablePrometheusMetricLogging $ addDatastoreLatency storeType operationName latency
  pure res
