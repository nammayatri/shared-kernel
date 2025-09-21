{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.External.MultiModal.Interface.Types
  ( module Kernel.External.MultiModal.Interface.Types,
    GeneralVehicleType (..),
  )
where

import Data.Aeson
import Data.OpenApi hiding (name)
import Data.Time (UTCTime)
import Deriving.Aeson
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import qualified Kernel.External.Maps.Google.Config as Google
import qualified Kernel.External.Maps.Google.MapsClient.Types as GT
import Kernel.External.MultiModal.OpenTripPlanner.Config (GeneralVehicleType)
import qualified Kernel.External.MultiModal.OpenTripPlanner.Config as OTP
import qualified Kernel.External.MultiModal.OpenTripPlanner.Types as OTPTypes
import qualified Kernel.Types.Distance as Distance
import qualified Kernel.Types.Time as Time
import Kernel.Utils.TH (mkHttpInstancesForEnum)

newtype MultiModalResponse = MultiModalResponse {routes :: [MultiModalRoute]}
  deriving (Show, Generic)
  deriving newtype (FromJSON, ToJSON, ToSchema)

data MultiModalRoute = MultiModalRoute
  { distance :: Distance.Distance,
    duration :: Time.Seconds,
    startTime :: Maybe UTCTime,
    endTime :: Maybe UTCTime,
    legs :: [MultiModalLeg],
    relevanceScore :: Maybe Double
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data NormalizerData = NormalizerData
  { minArrivalTime :: Maybe UTCTime,
    maxArrivalTime :: Maybe UTCTime,
    minDuration :: Maybe Time.Seconds,
    maxDuration :: Maybe Time.Seconds,
    minTransfers :: Maybe Int,
    maxTransfers :: Maybe Int,
    minCost :: Maybe Double,
    maxCost :: Maybe Double
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
    alternateShortNames :: [Text],
    color :: Maybe Text,
    fromStopDetails :: Maybe MultiModalStopDetails,
    toStopDetails :: Maybe MultiModalStopDetails,
    startLocation :: GT.LocationV2,
    endLocation :: GT.LocationV2,
    subLegOrder :: Int,
    fromArrivalTime :: Maybe UTCTime,
    fromDepartureTime :: Maybe UTCTime,
    toArrivalTime :: Maybe UTCTime,
    toDepartureTime :: Maybe UTCTime
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
    routeDetails :: [MultiModalRouteDetails],
    serviceTypes :: [Text],
    agency :: Maybe MultiModalAgency,
    fromArrivalTime :: Maybe UTCTime,
    fromDepartureTime :: Maybe UTCTime,
    toArrivalTime :: Maybe UTCTime,
    toDepartureTime :: Maybe UTCTime,
    entrance :: Maybe MultiModalLegGate,
    exit :: Maybe MultiModalLegGate
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data MultiModalLegGate = MultiModalLegGate
  { distance :: Maybe Double,
    lon :: Maybe Double,
    lat :: Maybe Double,
    isEntrance :: Maybe Bool,
    absoluteDirection :: Maybe OTPTypes.AbsoluteDirection,
    streetName :: Maybe Text,
    exit :: Maybe Text,
    stayOn :: Maybe Bool,
    area :: Maybe Bool,
    bogusName :: Maybe Bool,
    walkingBike :: Maybe Bool
  }
  deriving (Generic, Show, Eq, ToSchema, FromJSON, ToJSON)

data MultiModalServiceConfig = GoogleTransitConfig Google.GoogleCfg | OTPTransitConfig OTP.OTPCfg
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[SumTaggedObject "tag" "content"] MultiModalServiceConfig

$(mkHttpInstancesForEnum ''GeneralVehicleType)

$(mkBeamInstancesForEnumAndList ''GeneralVehicleType)

data SortingType
  = Fastest
  | MinimumTransits
  | MostRelevant
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Read, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''SortingType)

$(mkBeamInstancesForEnumAndList ''SortingType)

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
    maxAllowedPublicTransportLegs :: Int,
    sortingType :: SortingType
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)
