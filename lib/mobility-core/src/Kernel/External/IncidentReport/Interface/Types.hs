{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.External.IncidentReport.Interface.Types
  ( module Kernel.External.IncidentReport.Interface.Types,
  )
where

import Deriving.Aeson
import qualified Kernel.External.IncidentReport.ERSS.Config as ERSS
import Kernel.Prelude

newtype IncidentReportServiceConfig = ERSSConfig ERSS.ERSSCfg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data IncidentReportRes = Success | Fail | Unknown
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

data RouteMap = RouteMap
  { latitude :: Maybe Double,
    longitude :: Double
  }
  deriving (Generic, Show)

data TripInfo = TripInfo
  { origin :: Text,
    destination :: Text,
    bookingTime :: Text,
    vehicleRegNo :: Text,
    driverName :: Text,
    driverMobNo :: Text
  }
  deriving (Generic, Show)

data IncidentReportReq = IncidentReportReq
  { type_ :: Text,
    mobileNo :: Integer,
    travellerName :: Text,
    gender :: Text, -- String (“M” / “F”)
    age :: Maybe Int,
    address :: Maybe Text, -- Traveller Registered address
    incidentId :: Text,
    incidentDateTime :: Integer, -- Incident date time in milliseconds
    incidentInfo :: Text,
    tripInfo :: Maybe TripInfo,
    latitude :: Double,
    longitude :: Double,
    gpsPacketTime :: Text,
    routeMap :: Maybe [RouteMap],
    incidentLocation :: Text,
    district :: Text,
    state :: Text
  }
  deriving (Generic, Show)
