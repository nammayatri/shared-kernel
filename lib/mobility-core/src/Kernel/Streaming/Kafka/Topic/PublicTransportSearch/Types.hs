module Kernel.Streaming.Kafka.Topic.PublicTransportSearch.Types where

import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import qualified Kernel.Streaming.Kafka.Consumer as Cons
import Kernel.Streaming.Kafka.Consumer.Types (HasKafkaConsumer, KafkaConsumerTools)
import Kernel.Streaming.Kafka.HasKafkaTopics (HasKafkaTopics (..))
import qualified Kernel.Streaming.Kafka.Producer as Prod
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Streaming.MonadConsumer (MonadConsumer (..))
import Kernel.Streaming.MonadProducer (MonadProducer (..))
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Logging

type HasKafkaPublicTransportSearchConsumer env r =
  ( HasKafkaConsumer env r,
    HasField "publicTransportSearch" env (KafkaConsumerTools PublicTransportSearch)
  )

type SearchId = Text

type PersonId = Text

data PublicTransportSearch = PublicTransportSearch
  { id :: SearchId,
    gps :: LatLong,
    requestorId :: PersonId,
    createdAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON)

instance HasKafkaTopics PublicTransportSearch where
  getTopics = ["public_transport_bap_search"]

instance (Log (FlowR r), HasKafkaProducer r) => MonadProducer PublicTransportSearch (FlowR r) where
  type Args PublicTransportSearch = ()
  produceMessage () value = mapM_ ($ value) (Prod.produceMessage <$> map (,Nothing) (getTopics @PublicTransportSearch))

instance (Log (FlowR r), HasKafkaPublicTransportSearchConsumer env r) => MonadConsumer PublicTransportSearch (FlowR r) where
  receiveMessage = asks (.kafkaConsumerEnv.publicTransportSearch) >>= Cons.receiveMessage
