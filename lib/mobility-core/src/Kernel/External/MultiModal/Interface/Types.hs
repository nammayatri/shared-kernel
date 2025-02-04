{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.External.MultiModal.Interface.Types where

import Data.Aeson
import Data.OpenApi hiding (name)
import Data.Time (UTCTime)
import Deriving.Aeson
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import qualified Kernel.External.Maps.Google.Config as Google
import qualified Kernel.External.Maps.Google.MapsClient.Types as GT
import qualified Kernel.External.MultiModal.OpenTripPlanner.Config as OTP
import qualified Kernel.External.MultiModal.OpenTripPlanner.Types as OTPTypes
import qualified Kernel.Types.Distance as Distance
import qualified Kernel.Types.Time as Time
import Kernel.Utils.TH (mkHttpInstancesForEnum)

newtype MultiModalResponse = MultiModalResponse {routes :: [MultiModalRoute]}
  deriving (Show, Generic)

data MultiModalRoute = MultiModalRoute
  { distance :: Distance.Distance,
    duration :: Time.Seconds,
    startTime :: Maybe UTCTime,
    endTime :: Maybe UTCTime,
    legs :: [MultiModalLeg]
  }
  deriving (Show, Generic)

data MultiModalStopDetails = MultiModalStopDetails
  { stopCode :: Maybe Text,
    platformCode :: Maybe Text,
    name :: Maybe Text,
    gtfsId :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data MultiModalAgency = MultiModalAgency
  { gtfsId :: Maybe Text,
    name :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data MultiModalRouteDetails = MultiModalRouteDetails
  { gtfsId :: Maybe Text,
    longName :: Maybe Text,
    shortName :: Maybe Text,
    color :: Maybe Text,
    frequency :: Maybe Time.Seconds
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data MultiModalLeg = MultiModalLeg
  { distance :: Distance.Distance,
    duration :: Time.Seconds,
    polyline :: GT.Polyline,
    mode :: GeneralVehicleType,
    startLocation :: GT.LocationV2,
    endLocation :: GT.LocationV2,
    fromStopDetails :: Maybe MultiModalStopDetails,
    toStopDetails :: Maybe MultiModalStopDetails,
    routeDetails :: Maybe MultiModalRouteDetails,
    agency :: Maybe MultiModalAgency,
    fromArrivalTime :: Maybe UTCTime,
    fromDepartureTime :: Maybe UTCTime,
    toArrivalTime :: Maybe UTCTime,
    toDepartureTime :: Maybe UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data MultiModalServiceConfig = GoogleTransitConfig Google.GoogleCfg | OTPTransitConfig OTP.OTPCfg
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[SumTaggedObject "tag" "content"] MultiModalServiceConfig

data GeneralVehicleType
  = Bus
  | MetroRail
  | Walk
  | Subway
  | Unspecified
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Read, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''GeneralVehicleType)

$(mkBeamInstancesForEnumAndList ''GeneralVehicleType)

data GetTransitRoutesReq = GetTransitRoutesReq
  { origin :: GT.WayPointV2,
    destination :: GT.WayPointV2,
    arrivalTime :: Maybe UTCTime,
    departureTime :: Maybe UTCTime,
    mode :: Maybe GT.ModeV2,
    transitPreferences :: Maybe GT.TransitPreferencesV2,
    transportModes :: Maybe [Maybe OTPTypes.TransportMode],
    minimumWalkDistance :: Distance.Meters,
    permissibleModes :: [GeneralVehicleType],
    maxAllowedPublicTransportLegs :: Int
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)
