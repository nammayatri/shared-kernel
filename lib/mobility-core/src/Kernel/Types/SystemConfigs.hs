module Kernel.Types.SystemConfigs where

import Kernel.Prelude

data SystemConfigs = SystemConfigs
  { id :: Text,
    configValue :: Text
  }
  deriving (Generic)
