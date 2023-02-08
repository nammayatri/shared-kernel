module Kernel.Streaming.Kafka.Topic.PublicTransportSearch.Functions where

import Kernel.Streaming.Kafka.Topic.PublicTransportSearch.Types
import Kernel.Streaming.MonadProducer (MonadProducer (..))

producePublicTransportSearchMessage ::
  ( MonadProducer PublicTransportSearch m
  ) =>
  PublicTransportSearch ->
  m ()
producePublicTransportSearchMessage = do
  produceMessage ()
