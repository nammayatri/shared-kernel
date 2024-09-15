{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Kernel.External.MultiModal.Types where

import Data.Time (UTCTime)
import EulerHS.Prelude
import qualified Kernel.External.Maps.Google.MapsClient.Types as GT

newtype MultiModalResponse = MultiModalResponse {routes :: [MultiModalRoute]}
  deriving (Generic)

data MultiModalRoute = MultiModalRoute
  { distance :: Double,
    duration :: Int,
    legs :: [MultiModalLeg]
  }
  deriving (Generic)

data MutliModalStopDetails = MultiModalStopDetails
  { stopCode :: Maybe String,
    name :: Maybe Text
  }
  deriving (Show, Generic)

data MultiModalAgency = MultiModalAgency
  { gtfsId :: Maybe Text,
    name :: Text
  }
  deriving (Generic)

data MultiModalLeg = MultiModalLeg
  { distance :: Double,
    duration :: Double,
    polyline :: GT.Polyline,
    mode :: String,
    startLocation :: GT.LocationV2,
    endLocation :: GT.LocationV2,
    fromStopDetails :: Maybe MutliModalStopDetails,
    toStopDetails :: Maybe MutliModalStopDetails,
    agency :: Maybe MultiModalAgency,
    fromArrivalTime :: Maybe UTCTime,
    fromDepartureTime :: Maybe UTCTime,
    toArrivalTime :: Maybe UTCTime,
    toDepartureTime :: Maybe UTCTime
  }
  deriving (Generic)
