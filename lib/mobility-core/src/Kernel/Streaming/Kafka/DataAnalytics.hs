{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Streaming.Kafka.DataAnalytics where

import EulerHS.Prelude
import Kernel.Streaming.Kafka.Commons (KafkaTopic)
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Common
import Kernel.Utils.Common

streamToKafka ::
  ( MonadFlow m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text],
    ToJSON req,
    ToJSON res
  ) =>
  RequestResponseData req res ->
  Text ->
  KafkaTopic ->
  m ()
streamToKafka dataForAnalytics someId topic = fork "stream data to clickhouse" $ do
  appPrefix <- asks (.appPrefix)
  now <- getCurrentTime
  let kafkaKey = appPrefix <> ":" <> someId <> ":" <> show now
  produceMessage (topic, Just (encodeUtf8 kafkaKey)) dataForAnalytics
  logInfo "Stream sended"

data RequestResponseData req res = RequestResponseData
  { request :: req,
    response :: res
  }
  deriving (Generic, Show, ToJSON, FromJSON)

withDataStreaming ::
  ( MonadFlow m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text],
    ToJSON req,
    ToJSON res
  ) =>
  req ->
  (req -> m res) ->
  Text ->
  KafkaTopic ->
  m res
withDataStreaming request reqFunq someId topic = do
  response <- reqFunq request
  streamToKafka (RequestResponseData {..}) someId topic
  return response
