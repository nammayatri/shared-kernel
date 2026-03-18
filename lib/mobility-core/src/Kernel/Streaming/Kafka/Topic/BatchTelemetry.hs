{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Streaming.Kafka.Topic.BatchTelemetry where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Kernel.Streaming.Kafka.HasKafkaTopics (HasKafkaTopics (..))
import qualified Kernel.Streaming.Kafka.Producer as Prod
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Streaming.MonadProducer (MonadProducer (..))
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Logging

data BatchTelemetryEvent = BatchTelemetryEvent
  { batchTraceId :: Text,
    stage :: Text, -- "client" | "lts_drainer" | "haskell_backend" | "aggregator"
    pointCount :: Maybe Int,
    clientBatchedAt :: Maybe UTCTime,
    ltsReceivedAt :: Maybe UTCTime,
    ltsDrainedAt :: Maybe UTCTime,
    backendReceivedAt :: Maybe UTCTime,
    backendProcessedAt :: Maybe UTCTime,
    drainerQueueDepth :: Maybe Int,
    snapToRoadCalled :: Maybe Bool,
    merchantId :: Maybe Text,
    cityId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

instance HasKafkaTopics BatchTelemetryEvent where
  getTopics = ["batch_telemetry_events"]

instance (Log (FlowR r), HasKafkaProducer r) => MonadProducer BatchTelemetryEvent (FlowR r) where
  type Args BatchTelemetryEvent = Maybe KafkaKey
  produceMessage key value = mapM_ ($ value) (Prod.produceMessage <$> map (,key) (getTopics @BatchTelemetryEvent))
