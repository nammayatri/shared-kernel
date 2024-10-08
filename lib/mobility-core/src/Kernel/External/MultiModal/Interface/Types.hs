{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.External.MultiModal.Interface.Types where

import Data.OpenApi hiding (name)
import Data.Time (UTCTime)
import Deriving.Aeson
import EulerHS.Prelude
import qualified Kernel.External.Maps.Google.Config as Google
import qualified Kernel.External.Maps.Google.MapsClient.Types as GT
import qualified Kernel.External.MultiModal.OpenTripPlanner.Config as OTP
import qualified Kernel.External.MultiModal.OpenTripPlanner.Types as OTPTypes

newtype MultiModalResponse = MultiModalResponse {routes :: [MultiModalRoute]}
  deriving (Show, Generic)

data MultiModalRoute = MultiModalRoute
  { distance :: Double,
    duration :: Int,
    legs :: [MultiModalLeg]
  }
  deriving (Show, Generic)

data MultiModalStopDetails = MultiModalStopDetails
  { stopCode :: Maybe String,
    name :: Maybe Text
  }
  deriving (Show, Generic)

data MultiModalAgency = MultiModalAgency
  { gtfsId :: Maybe Text,
    name :: Text
  }
  deriving (Show, Generic)

data MultiModalLeg = MultiModalLeg
  { distance :: Double,
    duration :: Double,
    polyline :: GT.Polyline,
    mode :: String,
    startLocation :: GT.LocationV2,
    endLocation :: GT.LocationV2,
    fromStopDetails :: Maybe MultiModalStopDetails,
    toStopDetails :: Maybe MultiModalStopDetails,
    agency :: Maybe MultiModalAgency,
    fromArrivalTime :: Maybe UTCTime,
    fromDepartureTime :: Maybe UTCTime,
    toArrivalTime :: Maybe UTCTime,
    toDepartureTime :: Maybe UTCTime
  }
  deriving (Show, Generic)

data MultiModalServiceConfig = GoogleTransitConfig Google.GoogleCfg | OTPTransitConfig OTP.OTPCfg
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[SumTaggedObject "tag" "content"] MultiModalServiceConfig

data GetTransitRoutesReq = GetTransitRoutesReq
  { origin :: GT.WayPointV2,
    destination :: GT.WayPointV2,
    arrivalTime :: Maybe UTCTime,
    departureTime :: Maybe UTCTime,
    mode :: Maybe GT.ModeV2,
    transitPreferences :: Maybe GT.TransitPreferencesV2,
    transportModes :: Maybe [Maybe OTPTypes.TransportMode]
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)
