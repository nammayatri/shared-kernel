module Kernel.Streaming.MonadConsumer where

import Kernel.Prelude

class MonadConsumer a m where
  receiveMessage :: m (Maybe a)
