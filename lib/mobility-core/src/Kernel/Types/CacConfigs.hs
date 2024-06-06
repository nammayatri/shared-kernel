module Kernel.Types.CacConfigs where

import Kernel.Prelude

data CacConfigs = CacConfigs
  { id :: Text,
    configValue :: Text
  }
  deriving (Generic, Show)
