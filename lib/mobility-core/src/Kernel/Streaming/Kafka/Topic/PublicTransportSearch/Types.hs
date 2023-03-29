{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Streaming.Kafka.Topic.PublicTransportSearch.Types where

import Kernel.Types.CommonImport
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
