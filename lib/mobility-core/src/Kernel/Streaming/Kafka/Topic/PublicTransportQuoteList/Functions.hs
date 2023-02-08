module Kernel.Streaming.Kafka.Topic.PublicTransportQuoteList.Functions where

import Kernel.Streaming.Kafka.Topic.PublicTransportQuoteList.Types
import Kernel.Streaming.MonadProducer (MonadProducer (..))
import EulerHS.Prelude

producePublicTransportQuoteListMessage ::
  ( MonadProducer PublicTransportQuoteList m
  ) =>
  TransactionId ->
  [PublicTransportQuote] ->
  m ()
producePublicTransportQuoteListMessage txnId = do
  produceMessage () . PublicTransportQuoteList txnId
