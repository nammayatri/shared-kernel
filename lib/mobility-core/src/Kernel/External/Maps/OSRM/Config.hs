module Kernel.External.Maps.OSRM.Config where

import Kernel.Prelude
import Kernel.Types.Common

data OSRMCfg = OSRMCfg
  { osrmUrl :: BaseUrl,
    radiusDeviation :: Maybe Meters
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
