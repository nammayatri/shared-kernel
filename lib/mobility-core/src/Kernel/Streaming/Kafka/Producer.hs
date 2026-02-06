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
    produceToSecondaryProducer,
    releaseKafkaProducerTools,
    (..=),
    A.Value (Object),
    A.emptyObject,
    shouldProduceSecondary,
  )
where

import Data.Aeson (encode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as LBS
import EulerHS.Prelude
import Kafka.Producer as KafkaProd
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Types.Error
import Kernel.Types.Logging
import Kernel.Utils.Error.Throwing (throwError)
import Kernel.Utils.Logging (logError)
import System.Environment (lookupEnv)

type KPartitionId = Int

shouldProduceSecondary :: IO Bool
shouldProduceSecondary = fromMaybe False . (readMaybe =<<) <$> lookupEnv "PRODUCE_SECONDARY_KAFKA"

produceMessage :: (Log m, MonadThrow m, MonadIO m, MonadReader r m, HasKafkaProducer r, ToJSON a) => (KafkaTopic, Maybe KafkaKey) -> a -> m ()
produceMessage (topic, key) event = produceMessageImpl (topic, key) event Nothing

produceMessageInPartition :: (Log m, MonadThrow m, MonadIO m, MonadReader r m, HasKafkaProducer r, ToJSON a) => (KafkaTopic, Maybe KafkaKey) -> a -> KPartitionId -> m ()
produceMessageInPartition (topic, key) event partitionId = produceMessageImpl (topic, key) event $ Just partitionId

-- | Push to secondary Kafka producer if present. Calls onError with message on failure (does not throw).
produceToSecondaryProducer :: (MonadIO m) => Maybe KafkaProd.KafkaProducer -> KafkaProd.ProducerRecord -> (Text -> m ()) -> m ()
produceToSecondaryProducer mbSecondaryProducer record onError = do
  shouldProduce <- liftIO shouldProduceSecondary
  when shouldProduce $
    whenJust mbSecondaryProducer $ \secondaryProd -> do
      mbErr <- liftIO $ KafkaProd.produceMessage secondaryProd record
      whenJust mbErr $ \err ->
        onError $ "Secondary Kafka produce failed: " <> show err

produceMessageImpl :: (Log m, MonadThrow m, MonadIO m, MonadReader r m, HasKafkaProducer r, ToJSON a) => (KafkaTopic, Maybe KafkaKey) -> a -> Maybe KPartitionId -> m ()
produceMessageImpl (topic, key) event mbPartitionId = do
  when (null topic) $ throwM KafkaTopicIsEmptyString
  kafkaProducerTools <- asks (.kafkaProducerTools)
  mbErr <- KafkaProd.produceMessage kafkaProducerTools.producer message
  whenJust mbErr (throwError . KafkaUnableToProduceMessage)
  produceToSecondaryProducer kafkaProducerTools.secondaryProducer message logError
  where
    message =
      ProducerRecord
        { prTopic = TopicName topic,
          prPartition = maybe UnassignedPartition SpecifiedPartition mbPartitionId,
          prKey = key,
          prValue = Just . LBS.toStrict $ encode event
        }

(..=) :: ToJSON a => AesonKey.Key -> a -> AKM.KeyMap A.Value
(..=) = (A..=)
