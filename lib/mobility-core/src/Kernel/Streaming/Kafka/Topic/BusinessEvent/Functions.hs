module Kernel.Streaming.Kafka.Topic.BusinessEvent.Functions where

import EulerHS.Prelude
import Kernel.Streaming.Kafka.Topic.BusinessEvent.Environment
import Kernel.Streaming.Kafka.Topic.BusinessEvent.Types
import Kernel.Streaming.MonadProducer (MonadProducer (..))
import Kernel.Types.Logging
import Kernel.Types.Time

produceBusinessEventMessage ::
  ( MonadProducer BusinessEvent m,
    Log m,
    MonadTime m,
    MonadReader r m,
    HasKafkaBE r kafkaEnvs,
    ToJSON a,
    ToJSON b
  ) =>
  KafkaBEName ->
  a ->
  b ->
  m ()
produceBusinessEventMessage eventName metadata payload = do
  event <- buildBusinessEvent eventName metadata payload
  produceMessage (Just $ encodeUtf8 event.eventName) event
