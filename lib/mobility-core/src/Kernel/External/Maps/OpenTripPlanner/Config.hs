module Kernel.External.Maps.OpenTripPlanner.Config where

import Kernel.Prelude

data OTPCfg = OTPCfg
  { baseUrl :: String
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
