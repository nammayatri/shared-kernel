{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

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
import qualified Kernel.Types.Common as KTC
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

addMaxMessages :: ProducerProperties -> [KTC.KafkaProperties] -> ProducerProperties
addMaxMessages props kafkaProperties =
  let propertyList = map (\kv -> (KTC.propName kv, KTC.propValue kv)) kafkaProperties
      updatedKafkaProps = foldl (\acc (k, v) -> Map.insert k v acc) (ppKafkaProps props) propertyList
   in props {ppKafkaProps = updatedKafkaProps}

castCompression :: KafkaCompression -> KafkaCompressionCodec
castCompression kafkaCompression =
  case kafkaCompression of
    NO_COMPRESSION -> NoCompression
    GZIP -> Gzip
    SNAPPY -> Snappy
    LZ4 -> Lz4

buildKafkaProducerTools' :: KafkaProducerCfg -> [KTC.KafkaProperties] -> IO KafkaProducerTools
buildKafkaProducerTools' kafkaProducerCfg kafkaProperties = do
  producer <- newProducer (addMaxMessages (producerProps kafkaProducerCfg) kafkaProperties) >>= either (throwM . KafkaUnableToBuildTools) return
  return $ KafkaProducerTools {..}

buildKafkaProducerTools :: KafkaProducerCfg -> IO KafkaProducerTools
buildKafkaProducerTools kafkaProducerCfg = do
  producer <- newProducer (producerProps kafkaProducerCfg) >>= either (throwM . KafkaUnableToBuildTools) return
  return $ KafkaProducerTools {..}

releaseKafkaProducerTools :: KafkaProducerTools -> IO ()
releaseKafkaProducerTools kafkaProducerTools = closeProducer kafkaProducerTools.producer
