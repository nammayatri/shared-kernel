module Kernel.External.MultiModal.OpenTripPlanner.Config where

import Data.Hashable (Hashable)
import Kernel.Prelude

data MultiModalWeightedSortCfg = MultiModalWeightedSortCfg
  { arrivalTime :: Double,
    duration :: Double,
    transfers :: Double,
    cost :: Maybe Double,
    perModeCost :: Maybe [(GeneralVehicleType, Double)]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

validateWeightedSortCfg :: MultiModalWeightedSortCfg -> Bool
validateWeightedSortCfg MultiModalWeightedSortCfg {..} =
  let totalWeight =
        arrivalTime
          + duration
          + transfers
          + (fromMaybe 0.0 cost)
   in totalWeight == 1

data OTPQuery = NORMAL | MULTI_SEARCH
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data OTPCfg = OTPCfg
  { baseUrl :: BaseUrl,
    queryType :: Maybe OTPQuery,
    nightMode :: Maybe Bool,
    numItineraries :: Maybe Int,
    weightedSortCfg :: MultiModalWeightedSortCfg
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Put here due to cyclic import issue
data GeneralVehicleType
  = Bus
  | MetroRail
  | Walk
  | Subway
  | Unspecified
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Read, ToSchema, ToParamSchema, Hashable)
