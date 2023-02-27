{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Streaming.Kafka.Producer
  ( buildKafkaProducerTools,
    Kernel.Streaming.Kafka.Producer.produceMessage,
    produceMessageInPartition,
    releaseKafkaProducerTools,
    (..=),
    A.Value (Object),
    A.emptyObject,
  )
where

import Data.Aeson (encode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as HM
import EulerHS.Prelude
import Kafka.Producer as KafkaProd
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Types.Error
import Kernel.Types.Logging
import Kernel.Utils.Error.Throwing (throwError)

type KPartitionId = Int

produceMessage :: (Log m, MonadThrow m, MonadIO m, MonadReader r m, HasKafkaProducer r, ToJSON a) => (KafkaTopic, Maybe KafkaKey) -> a -> m ()
produceMessage (topic, key) event = produceMessageImpl (topic, key) event Nothing

produceMessageInPartition :: (Log m, MonadThrow m, MonadIO m, MonadReader r m, HasKafkaProducer r, ToJSON a) => (KafkaTopic, Maybe KafkaKey) -> a -> KPartitionId -> m ()
produceMessageInPartition (topic, key) event partitionId = produceMessageImpl (topic, key) event $ Just partitionId

produceMessageImpl :: (Log m, MonadThrow m, MonadIO m, MonadReader r m, HasKafkaProducer r, ToJSON a) => (KafkaTopic, Maybe KafkaKey) -> a -> Maybe KPartitionId -> m ()
produceMessageImpl (topic, key) event mbPartitionId = do
  when (null topic) $ throwM KafkaTopicIsEmptyString
  kafkaProducerTools <- asks (.kafkaProducerTools)
  mbErr <- KafkaProd.produceMessage kafkaProducerTools.producer message
  whenJust mbErr (throwError . KafkaUnableToProduceMessage)
  where
    message =
      ProducerRecord
        { prTopic = TopicName topic,
          prPartition = maybe UnassignedPartition SpecifiedPartition mbPartitionId,
          prKey = key,
          prValue = Just . LBS.toStrict $ encode event
        }

(..=) :: ToJSON a => Text -> a -> HM.HashMap Text A.Value
(..=) = (A..=)
