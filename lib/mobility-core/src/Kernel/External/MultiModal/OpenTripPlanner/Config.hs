module Kernel.External.MultiModal.OpenTripPlanner.Config (OTPCfg) where

import Kernel.Prelude

data OTPCfg = OTPCfg
  { baseUrl :: String
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
