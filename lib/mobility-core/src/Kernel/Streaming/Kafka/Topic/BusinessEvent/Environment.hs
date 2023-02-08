module Kernel.Streaming.Kafka.Topic.BusinessEvent.Environment where

import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Utils.App (getPodName)

type HasKafkaBE r kafkaEnvs = (HasField "kafkaEnvs" r kafkaEnvs, HasField "businessEventEnv" kafkaEnvs KafkaBEEnv)

data KafkaBEEnv = KafkaBEEnv
  { hostName :: KafkaHostName,
    serviceName :: KafkaServiceName
  }
  deriving (Generic)

buildKafkaBEEnv :: KafkaServiceName -> IO KafkaBEEnv
buildKafkaBEEnv serviceName = do
  hostName <- getPodName
  return $
    KafkaBEEnv
      { ..
      }
