{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Streaming.Kafka.Consumer.Types
  ( KafkaConsumerCfg (..),
    KafkaConsumerTools,
    HasKafkaConsumer,
    buildKafkaConsumerTools,
    releaseKafkaConsumerTools,
    module Reexport,
  )
where

import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import Kafka.Consumer hiding (ConsumerGroupId, groupId)
import qualified Kafka.Consumer as Consumer
import Kernel.Streaming.Kafka.Commons as Reexport
import Kernel.Streaming.Kafka.HasKafkaTopics
import Kernel.Types.Error
import Kernel.Utils.Dhall (FromDhall)

type HasKafkaConsumer env r = HasField "kafkaConsumerEnv" r env

type ConsumerGroupId = Text

data KafkaConsumerCfg = KafkaConsumerCfg
  { brokers :: KafkaBrokersList,
    groupId :: ConsumerGroupId,
    kafkaCompression :: KafkaCompression,
    timeoutMilliseconds :: Int
  }
  deriving (Generic, FromDhall)

data KafkaConsumerTools a = KafkaConsumerTools
  { kafkaConsumerCfg :: KafkaConsumerCfg,
    consumer :: Consumer.KafkaConsumer
  }
  deriving (Generic)

consumerProps :: KafkaConsumerCfg -> ConsumerProperties
consumerProps kafkaConsumerCfg =
  brokersList castBrokers
    <> Consumer.groupId (Consumer.ConsumerGroupId kafkaConsumerCfg.groupId)
    <> logLevel KafkaLogDebug
    <> compression castCompression
  where
    castBrokers = BrokerAddress <$> kafkaConsumerCfg.brokers
    castCompression =
      case kafkaConsumerCfg.kafkaCompression of
        NO_COMPRESSION -> NoCompression
        GZIP -> Gzip
        SNAPPY -> Snappy
        LZ4 -> Lz4

consumerSub :: [KafkaTopic] -> Subscription
consumerSub topicList =
  Consumer.topics castTopics
    <> offsetReset Earliest
  where
    castTopics = TopicName <$> topicList

buildKafkaConsumerTools :: forall a. HasKafkaTopics a => KafkaConsumerCfg -> IO (KafkaConsumerTools a)
buildKafkaConsumerTools kafkaConsumerCfg = do
  consumer <-
    newConsumer (consumerProps kafkaConsumerCfg) (consumerSub $ getTopics @a)
      >>= either (throwM . KafkaUnableToBuildTools) return

  return $ KafkaConsumerTools {..}

releaseKafkaConsumerTools :: KafkaConsumerTools a -> IO ()
releaseKafkaConsumerTools kafkaConsumerTools =
  closeConsumer kafkaConsumerTools.consumer
    >>= flip whenJust (throwM . KafkaUnableToReleaseTools)
