module Kernel.External.MultiModal.OpenTripPlanner.Config where

import Kernel.Prelude

data OTPCfg = OTPCfg
 { baseUrl :: BaseUrl
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
