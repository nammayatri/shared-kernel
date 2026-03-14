{-# LANGUAGE DeriveGeneric #-}

-- | Shared types for Digital Twin integration across services.
-- Used by rider-app, dashboard, and other services that consume
-- fleet-state-aggregator data.
module Kernel.External.DigitalTwin
  ( -- * Bus State Types
    BusStatus (..),
    MovementType (..),
    ServiceType (..),
    RouteConfidence (..),

    -- * Alert Types
    AlertType (..),
    AlertSeverity (..),
    AlertStatus (..),

    -- * Corridor Types (Twin 2)
    CongestionLevel (..),

    -- * Headway Types
    HeadwayStatus (..),

    -- * Fleet Score Types (Twin 4)
    ScoreCategory (..),
  )
where

import Data.Aeson
import Kernel.Prelude

-- | Bus operational status
data BusStatus
  = OnRoute
  | Delayed
  | Deviated
  | Stationary
  | NoSignal
  | AtDepot
  deriving (Show, Eq, Ord, Generic, Read)

instance FromJSON BusStatus
instance ToJSON BusStatus

-- | Bus movement classification (Twin 1 FINAL-3)
data MovementType
  = RevenueService
  | DepotPullout
  | DepotPullin
  | Deadheading
  | OffRoute
  | StationaryMovement
  deriving (Show, Eq, Ord, Generic, Read)

instance FromJSON MovementType
instance ToJSON MovementType

-- | Bus service type (REV-10)
data ServiceType
  = Ordinary
  | Express
  | Deluxe
  | AC
  | Special
  deriving (Show, Eq, Ord, Generic, Read)

instance FromJSON ServiceType
instance ToJSON ServiceType

-- | Route match confidence from gps-processor
data RouteConfidence
  = HighConfidence
  | MediumConfidence
  | LowConfidence
  | NoConfidence
  deriving (Show, Eq, Ord, Generic, Read)

instance FromJSON RouteConfidence
instance ToJSON RouteConfidence

-- | Fleet alert types
data AlertType
  = BusStationary
  | RouteDeviation
  | GpsSignalLoss
  | HeadwayGap
  | HeadwayBunching
  | Overspeeding
  | ShortTurn
  deriving (Show, Eq, Ord, Generic, Read)

instance FromJSON AlertType
instance ToJSON AlertType

-- | Alert severity
data AlertSeverity
  = Critical
  | Warning
  | Info
  deriving (Show, Eq, Ord, Generic, Read)

instance FromJSON AlertSeverity
instance ToJSON AlertSeverity

-- | Alert lifecycle status
data AlertStatus
  = Active
  | Acknowledged
  | Resolved
  | Dismissed
  deriving (Show, Eq, Ord, Generic, Read)

instance FromJSON AlertStatus
instance ToJSON AlertStatus

-- | Corridor congestion level (Twin 2)
data CongestionLevel
  = FreeFlow    -- > 25 km/h
  | SlowMoving  -- 15-25 km/h
  | Congested   -- 8-15 km/h
  | Severe       -- < 8 km/h
  deriving (Show, Eq, Ord, Generic, Read)

instance FromJSON CongestionLevel
instance ToJSON CongestionLevel

-- | Headway status for route monitoring
data HeadwayStatus
  = HeadwayNormal
  | HeadwayWarning
  | HeadwayCritical
  | HeadwayInsufficientData
  deriving (Show, Eq, Ord, Generic, Read)

instance FromJSON HeadwayStatus
instance ToJSON HeadwayStatus

-- | Score category for fleet performance (Twin 4)
data ScoreCategory
  = Utilization
  | Efficiency
  | Adherence
  | Coverage
  deriving (Show, Eq, Ord, Generic, Read)

instance FromJSON ScoreCategory
instance ToJSON ScoreCategory
