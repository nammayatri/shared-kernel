{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Tools.Metrics.CoreMetrics.Types
  ( HasCoreMetrics,
    CoreMetrics (..),
    CoreMetricsContainer (..),
    DeploymentVersion (..),
    registerCoreMetricsContainer,
  )
where

import qualified EulerHS.KVConnector.Metrics as KVMetrics
import EulerHS.Prelude as E
import GHC.Records.Extra
import Kernel.Types.Time (Milliseconds, Seconds)
import Prometheus as P
import Servant.Client (BaseUrl, ClientError)

type RequestLatencyMetric = P.Vector P.Label4 P.Histogram

type DatastoresLatencyMetric = P.Vector P.Label3 P.Histogram

type ErrorCounterMetric = P.Vector P.Label4 P.Counter

type URLCallRetriesMetric = P.Vector P.Label3 P.Counter

type URLCallRetryFailuresMetric = P.Vector P.Label2 P.Counter

type SortedSetMetric = P.Vector P.Label2 P.Counter

type StreamMetric = P.Vector P.Label2 P.Counter

type GenericLatencyMetric = P.Vector P.Label2 P.Histogram

type SchedulerFailureMetric = P.Vector P.Label2 P.Counter

type GenericCounter = P.Vector P.Label1 P.Counter

type SystemConfigsFailedCounter = P.Vector P.Label1 P.Counter

type OpenTripPlannerResponseMetric = P.Vector P.Label4 P.Counter

type OpenTripPlannerLatencyMetric = P.Vector P.Label3 P.Histogram

newBuckets :: [Double]
newBuckets = [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 15, 20, 25, 30, 40, 50]

type HasCoreMetrics r =
  ( HasField "coreMetrics" r CoreMetricsContainer,
    HasField "version" r DeploymentVersion
  )

newtype DeploymentVersion = DeploymentVersion {getDeploymentVersion :: Text}

class CoreMetrics m where
  addRequestLatency :: Text -> Text -> Milliseconds -> Either ClientError a -> m ()
  addDatastoreLatency :: Text -> Text -> Milliseconds -> m ()
  incrementErrorCounter :: Text -> SomeException -> m ()
  addUrlCallRetries :: BaseUrl -> Int -> m ()
  addUrlCallRetryFailures :: BaseUrl -> m ()
  incrementSortedSetCounter :: Text -> m ()
  incrementStreamCounter :: Text -> m ()
  addGenericLatency :: Text -> Milliseconds -> m ()
  incrementSchedulerFailureCounter :: Text -> m ()
  incrementGenericMetrics :: Text -> m ()
  incrementSystemConfigsFailedCounter :: Text -> m ()
  addGenericLatencyMetrics :: Text -> Seconds -> m ()
  addOpenTripPlannerResponse :: Text -> Text -> Text -> m ()
  addOpenTripPlannerLatency :: Text -> Text -> Milliseconds -> m ()

data CoreMetricsContainer = CoreMetricsContainer
  { requestLatency :: RequestLatencyMetric,
    datastoresLatency :: DatastoresLatencyMetric,
    genericLatency :: GenericLatencyMetric,
    errorCounter :: ErrorCounterMetric,
    urlCallRetries :: URLCallRetriesMetric,
    urlCallRetryFailures :: URLCallRetryFailuresMetric,
    sortedSetCounter :: SortedSetMetric,
    streamCounter :: StreamMetric,
    schedulerFailureCounter :: SchedulerFailureMetric,
    genericCounter :: GenericCounter,
    systemConfigsFailedCounter :: SystemConfigsFailedCounter,
    kvRedisMetricsContainer :: KVMetrics.KVMetricHandler,
    genericLatencyMetrics :: GenericLatencyMetric,
    openTripPlannerResponseMetric :: OpenTripPlannerResponseMetric,
    openTripPlannerLatencyMetric :: OpenTripPlannerLatencyMetric
  }

registerCoreMetricsContainer :: IO CoreMetricsContainer
registerCoreMetricsContainer = do
  requestLatency <- registerRequestLatencyMetric
  datastoresLatency <- registerDatastoresLatencyMetrics
  genericLatency <- registerGenericLatencyMetrics
  errorCounter <- registerErrorCounterMetric
  urlCallRetries <- registerURLCallRetriesMetric
  urlCallRetryFailures <- registerURLCallRetryFailuresMetric
  sortedSetCounter <- registerSortedSetMetric
  streamCounter <- registerStreamCounter
  schedulerFailureCounter <- registerSchedulerFailureCounter
  genericCounter <- registerGenericCounter
  systemConfigsFailedCounter <- registerSystemConfigsFailedCounter
  kvRedisMetricsContainer <- KVMetrics.mkKVMetricHandler
  genericLatencyMetrics <- registerLatencyMetrics
  openTripPlannerResponseMetric <- registerOpenTripPlannerResponseMetric
  openTripPlannerLatencyMetric <- registerOpenTripPlannerLatencyMetric
  return CoreMetricsContainer {..}

registerDatastoresLatencyMetrics :: IO DatastoresLatencyMetric
registerDatastoresLatencyMetrics =
  P.register $
    P.vector ("datastore", "operation", "version") $
      P.histogram info P.defaultBuckets
  where
    info = P.Info "datastore_operation_duration" ""

registerRequestLatencyMetric :: IO RequestLatencyMetric
registerRequestLatencyMetric =
  P.register $
    P.vector ("host", "service", "status", "version") $
      P.histogram info P.defaultBuckets
  where
    info = P.Info "external_request_duration" ""

registerErrorCounterMetric :: IO ErrorCounterMetric
registerErrorCounterMetric =
  P.register $
    P.vector ("HttpCode", "ErrorContext", "ErrorCode", "version") $
      P.counter info
  where
    info = P.Info "error_counter" ""

registerURLCallRetriesMetric :: IO URLCallRetriesMetric
registerURLCallRetriesMetric =
  P.register $
    P.vector ("URL", "RetryCount", "version") $
      P.counter info
  where
    info = P.Info "url_call_retries_counter" ""

registerURLCallRetryFailuresMetric :: IO URLCallRetryFailuresMetric
registerURLCallRetryFailuresMetric =
  P.register $
    P.vector ("URL", "version") $
      P.counter info
  where
    info = P.Info "url_call_retry_failures_counter" ""

registerSortedSetMetric :: IO SortedSetMetric
registerSortedSetMetric =
  P.register $
    P.vector ("job_type", "version") $
      P.counter info
  where
    info = P.Info "sortedset_scheduled_jobs_counter" ""

registerStreamCounter :: IO StreamMetric
registerStreamCounter =
  P.register $
    P.vector ("job_type", "version") $
      P.counter info
  where
    info = P.Info "stream_jobs_counter" ""

registerGenericLatencyMetrics :: IO GenericLatencyMetric
registerGenericLatencyMetrics =
  P.register $
    P.vector ("operation", "version") $
      P.histogram info newBuckets
  where
    info = P.Info "producer_operation_duration" ""

registerSchedulerFailureCounter :: IO SchedulerFailureMetric
registerSchedulerFailureCounter =
  P.register $
    P.vector ("scheduler_type", "version") $
      P.counter info
  where
    info = P.Info "scheduler_jobs_fail_counter" ""

registerGenericCounter :: IO GenericCounter
registerGenericCounter =
  P.register $
    P.vector "event" $
      P.counter info
  where
    info = P.Info "generic_counter" ""

registerSystemConfigsFailedCounter :: IO SystemConfigsFailedCounter
registerSystemConfigsFailedCounter =
  P.register $
    P.vector "event" $
      P.counter info
  where
    info = P.Info "system_configs_failed_counter" ""

registerLatencyMetrics :: IO GenericLatencyMetric
registerLatencyMetrics =
  P.register $
    P.vector ("Action", "version") $
      P.histogram info newBuckets
  where
    info = P.Info "generic_app_latency_metrics" ""

registerOpenTripPlannerResponseMetric :: IO OpenTripPlannerResponseMetric
registerOpenTripPlannerResponseMetric =
  P.register $
    P.vector ("query_type", "status", "error_code", "version") $
      P.counter info
  where
    info = P.Info "open_trip_planner_response_counter" "OpenTripPlanner response counter with query type, status, error code and version"

registerOpenTripPlannerLatencyMetric :: IO OpenTripPlannerLatencyMetric
registerOpenTripPlannerLatencyMetric =
  P.register $
    P.vector ("query_type", "status", "version") $
      P.histogram info newBuckets
  where
    info = P.Info "open_trip_planner_latency_seconds" "OpenTripPlanner request latency in seconds"
