module Kernel.External.ChallanSearch.Signzy.Types where

import Kernel.Prelude

newtype ChallanSearchReq = ChallanSearchReq
  { vehicleNumber :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype ChallanSearchResp = ChallanSearchResp
  { result :: ChallanSearchResult
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ChallanSearchResult = ChallanSearchResult
  { regNo :: Text,
    message :: Text,
    status_code :: Int,
    challanDetails :: [ChallanDetail],
    successfulSources :: [Text],
    failedSources :: [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ChallanDetail = ChallanDetail
  { challanNumber :: Text,
    challanStatus :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)
