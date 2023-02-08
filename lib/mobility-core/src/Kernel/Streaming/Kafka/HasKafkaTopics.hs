{-# LANGUAGE AllowAmbiguousTypes #-}

module Kernel.Streaming.Kafka.HasKafkaTopics where

import Kernel.Streaming.Kafka.Commons

class HasKafkaTopics a where
  getTopics :: [KafkaTopic]
