module Kernel.Streaming.Kafka.Topic.PublicTransportQuoteList.Types where

import Kernel.Prelude
import qualified Kernel.Streaming.Kafka.Consumer as Cons
import Kernel.Streaming.Kafka.Consumer.Types (HasKafkaConsumer, KafkaConsumerTools)
import Kernel.Streaming.Kafka.HasKafkaTopics (HasKafkaTopics (..))
import qualified Kernel.Streaming.Kafka.Producer as Prod
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Streaming.MonadConsumer (MonadConsumer (..))
import Kernel.Streaming.MonadProducer (MonadProducer (..))
import Kernel.Types.Common
import Kernel.Types.Flow (FlowR)

type HasKafkaPublicTransportQuotesConsumer env r =
  ( HasKafkaConsumer env r,
    HasField "publicTransportQuotes" env (KafkaConsumerTools PublicTransportQuoteList)
  )

type TransactionId = Text

data PublicTransportStation = PublicTransportStation
  { name :: Text,
    stationCode :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data PublicTransportQuote = PublicTransportQuote
  { id :: Text,
    description :: Text,
    fare :: Money,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStation :: PublicTransportStation,
    arrivalStation :: PublicTransportStation,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data PublicTransportQuoteList = PublicTransportQuoteList
  { transactionId :: TransactionId,
    quoteList :: [PublicTransportQuote]
  }
  deriving (Generic, FromJSON, ToJSON)

instance HasKafkaTopics PublicTransportQuoteList where
  getTopics = ["public_transport_bap_quote_list"]

instance (Log (FlowR r), HasKafkaProducer r) => MonadProducer PublicTransportQuoteList (FlowR r) where
  type Args PublicTransportQuoteList = ()
  produceMessage () value = mapM_ ($ value) (Prod.produceMessage <$> map (,Nothing) (getTopics @PublicTransportQuoteList))

instance (Log (FlowR r), HasKafkaPublicTransportQuotesConsumer env r) => MonadConsumer PublicTransportQuoteList (FlowR r) where
  receiveMessage = asks (.kafkaConsumerEnv.publicTransportQuotes) >>= Cons.receiveMessage
