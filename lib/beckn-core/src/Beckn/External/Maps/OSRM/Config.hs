module Beckn.External.Maps.OSRM.Config where

import Beckn.Prelude
import Beckn.Types.Common

data OSRMCfg = OSRMCfg
  { osrmUrl :: BaseUrl,
    radiusDeviation :: Maybe Meters
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
