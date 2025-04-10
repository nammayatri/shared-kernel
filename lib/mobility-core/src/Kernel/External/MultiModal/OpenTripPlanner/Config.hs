module Kernel.External.MultiModal.OpenTripPlanner.Config where

import Kernel.Prelude

data OTPQuery = NORMAL | MULTI_SEARCH
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data OTPCfg = OTPCfg
  { baseUrl :: BaseUrl,
    queryType :: Maybe OTPQuery,
    nightMode :: Maybe Bool,
    numItineraries :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
