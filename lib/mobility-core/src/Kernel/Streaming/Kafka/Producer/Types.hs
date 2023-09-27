{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Streaming.Kafka.Producer.Types
  ( KafkaProducerCfg (..),
    KafkaProducerTools,
    buildKafkaProducerTools,
    buildKafkaProducerTools',
    releaseKafkaProducerTools,
    castCompression,
    HasKafkaProducer,
    module Reexport,
  )
where

import qualified Data.Map as Map
import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import Kafka.Producer as Producer
import Kernel.Streaming.Kafka.Commons as Reexport
import Kernel.Types.Error
import Kernel.Utils.Dhall (FromDhall)

type HasKafkaProducer r = HasField "kafkaProducerTools" r KafkaProducerTools

data KafkaProducerCfg = KafkaProducerCfg
  { brokers :: KafkaBrokersList,
    kafkaCompression :: KafkaCompression
  }
  deriving (Generic, FromDhall)

newtype KafkaProducerTools = KafkaProducerTools
  { producer :: Producer.KafkaProducer
  }
  deriving (Generic)

producerProps :: KafkaProducerCfg -> ProducerProperties
producerProps kafkaProducerCfg =
  brokersList castBrokers
    <> compression (castCompression kafkaProducerCfg.kafkaCompression)
    <> logLevel KafkaLogDebug
  where
    castBrokers = BrokerAddress <$> kafkaProducerCfg.brokers

addMaxMessages :: ProducerProperties -> Text -> Text -> ProducerProperties
addMaxMessages props key value =
  let updatedKafkaProps = Map.insert key value (ppKafkaProps props)
   in props {ppKafkaProps = updatedKafkaProps}

castCompression :: KafkaCompression -> KafkaCompressionCodec
castCompression kafkaCompression =
  case kafkaCompression of
    NO_COMPRESSION -> NoCompression
    GZIP -> Gzip
    SNAPPY -> Snappy
    LZ4 -> Lz4

buildKafkaProducerTools' :: KafkaProducerCfg -> Text -> IO KafkaProducerTools
buildKafkaProducerTools' kafkaProducerCfg maxMessages = do
  producer <- newProducer (addMaxMessages (producerProps kafkaProducerCfg) "queue.buffering.max.messages" maxMessages) >>= either (throwM . KafkaUnableToBuildTools) return
  return $ KafkaProducerTools {..}

buildKafkaProducerTools :: KafkaProducerCfg -> IO KafkaProducerTools
buildKafkaProducerTools kafkaProducerCfg = do
  producer <- newProducer (producerProps kafkaProducerCfg) >>= either (throwM . KafkaUnableToBuildTools) return
  return $ KafkaProducerTools {..}

releaseKafkaProducerTools :: KafkaProducerTools -> IO ()
releaseKafkaProducerTools kafkaProducerTools = closeProducer kafkaProducerTools.producer
