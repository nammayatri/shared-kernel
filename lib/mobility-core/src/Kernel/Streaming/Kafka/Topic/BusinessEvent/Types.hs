module Kernel.Streaming.Kafka.Topic.BusinessEvent.Types where

import Kernel.Streaming.Kafka.HasKafkaTopics (HasKafkaTopics (..))
import qualified Kernel.Streaming.Kafka.Producer as Prod
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Streaming.Kafka.Topic.BusinessEvent.Environment
import Kernel.Streaming.MonadProducer (MonadProducer (..))
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Logging
import Kernel.Types.Time
import Data.Aeson (Value)
import Data.Time (UTCTime)
import EulerHS.Prelude
import GHC.Records.Extra (HasField)

type KafkaBEName = Text

type KafkaBEMetadata = Value

type KafkaBEPayload = Value

data BusinessEvent = BusinessEvent
  { timestamp :: UTCTime,
    eventName :: KafkaBEName,
    hostName :: KafkaHostName,
    serviceName :: KafkaServiceName, -- mobility BAP | BPP
    metadata :: KafkaBEMetadata,
    payload :: KafkaBEPayload
  }
  deriving (Generic, Show, FromJSON, ToJSON)

buildBusinessEvent :: (Log m, MonadTime m, MonadReader r m, HasKafkaBE r kafkaEnvs, ToJSON a, ToJSON b) => KafkaBEName -> a -> b -> m BusinessEvent
buildBusinessEvent eventName metadata payload = do
  kafkaBEEnv <- asks (.kafkaEnvs.businessEventEnv)
  currTime <- getCurrentTime
  return $
    BusinessEvent
      { timestamp = currTime,
        eventName,
        hostName = kafkaBEEnv.hostName,
        serviceName = kafkaBEEnv.serviceName,
        metadata = toJSON metadata,
        payload = toJSON payload
      }

instance HasKafkaTopics BusinessEvent where
  getTopics = ["beckn_business_events"]

instance (Log (FlowR r), HasKafkaProducer r, HasKafkaBE r kafkaEnvs) => MonadProducer BusinessEvent (FlowR r) where
  type Args BusinessEvent = (Maybe KafkaKey)
  produceMessage key value = mapM_ ($ value) (Prod.produceMessage <$> map (,key) (getTopics @BusinessEvent))
