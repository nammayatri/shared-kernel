module Kernel.External.ChallanSearch.Interface.Types
  ( module Kernel.External.ChallanSearch.Interface.Types,
  )
where

import Kernel.External.ChallanSearch.Signzy.Config as Signzy
import Kernel.Prelude

data ChallanSearchServiceConfig = SignzyChallanSearch Signzy.SignzyChallanSearchCfg
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype PendingChallanReq = PendingChallanReq
  { vehicleNumber :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype PendingChallanResp = PendingChallanResp
  { pendingChallanCount :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)
