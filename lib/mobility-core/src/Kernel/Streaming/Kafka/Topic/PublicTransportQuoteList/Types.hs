 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
