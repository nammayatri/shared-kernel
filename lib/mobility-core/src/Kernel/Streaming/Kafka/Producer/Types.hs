module Kernel.Streaming.Kafka.Producer.Types
  ( KafkaProducerCfg (..),
    KafkaProducerTools,
    buildKafkaProducerTools,
    releaseKafkaProducerTools,
    HasKafkaProducer,
    module Reexport,
  )
where

import Kernel.Streaming.Kafka.Commons as Reexport
import Kernel.Types.Error
import Kernel.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import Kafka.Producer as Producer

type HasKafkaProducer r = HasField "kafkaProducerTools" r KafkaProducerTools

newtype KafkaProducerCfg = KafkaProducerCfg
  { brokers :: KafkaBrokersList
  }
  deriving (Generic, FromDhall)

newtype KafkaProducerTools = KafkaProducerTools
  { producer :: Producer.KafkaProducer
  }
  deriving (Generic)

producerProps :: KafkaProducerCfg -> ProducerProperties
producerProps kafkaProducerCfg =
  brokersList castBrokers
    <> logLevel KafkaLogDebug
  where
    castBrokers = BrokerAddress <$> kafkaProducerCfg.brokers

buildKafkaProducerTools :: KafkaProducerCfg -> IO KafkaProducerTools
buildKafkaProducerTools kafkaProducerCfg = do
  producer <- newProducer (producerProps kafkaProducerCfg) >>= either (throwM . KafkaUnableToBuildTools) return
  return $ KafkaProducerTools {..}

releaseKafkaProducerTools :: KafkaProducerTools -> IO ()
releaseKafkaProducerTools kafkaProducerTools = closeProducer kafkaProducerTools.producer
