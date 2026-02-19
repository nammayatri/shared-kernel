{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Kernel.External.SOS.Interface.Types
  ( SOSHandler (..),
    SOSServiceConfig (..),
    InitialSOSReq (..),
    InitialSOSRes (..),
    SOSTraceReq (..),
    SOSTraceRes (..),
    SOSStatusUpdateReq (..),
    SOSStatusUpdateRes (..),
  )
where

import Data.Aeson
import Deriving.Aeson
import qualified Kernel.External.SOS.ERSS.Config as ERSSConfig
import qualified Kernel.External.SOS.GJ112.Config as GJ112Config
import Kernel.Prelude

-- | Handler pattern for SOS services
data SOSHandler m = SOSHandler
  { getProviderConfig :: m SOSServiceConfig
  }

-- | Configuration sum type for all SOS providers
data SOSServiceConfig
  = ERSSConfig ERSSConfig.ERSSCfg
  | GJ112Config GJ112Config.GJ112Cfg
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[SumTaggedObject "tag" "content"] SOSServiceConfig

-- | Unified Initial SOS Request (common across all providers)
data InitialSOSReq = InitialSOSReq
  { sosId :: Maybe Text,
    dateTime :: Text,
    latitude :: Double,
    longitude :: Double,
    speed :: Maybe Double,
    mobileNo :: Text,
    imeiNo :: Maybe Text,
    gpsProvider :: Maybe Text,
    senderName :: Maybe Text,
    address :: Maybe Text,
    gpsAccuracy :: Maybe Double,
    stateCode :: Maybe Text,
    silentCommunication :: Maybe Bool,
    specialNeeds :: Maybe Text,
    dob :: Maybe Text,
    gender :: Maybe Text,
    attachmentFileName :: Maybe Text,
    driverName :: Maybe Text,
    driverContactNo :: Maybe Text,
    vehicleNo :: Maybe Text,
    vehicleModel :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleType :: Maybe Text,
    vehicleMake :: Maybe Text,
    vehicleAppearanceNotes :: Maybe Text,
    vehicleLat :: Maybe Double,
    vehicleLon :: Maybe Double,
    vehicleLocationUrl :: Maybe Text,
    videoPath :: Maybe Text,
    emergencyContact1Name :: Maybe Text,
    emergencyContact1Phone :: Maybe Text,
    emergencyContact2Name :: Maybe Text,
    emergencyContact2Phone :: Maybe Text,
    city :: Maybe Text,
    emergencyMessage :: Maybe Text,
    email :: Maybe Text,
    vendorName :: Maybe Text,
    deviceType :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON InitialSOSReq

instance FromJSON InitialSOSReq

instance ToSchema InitialSOSReq

-- | Unified Initial SOS Response
data InitialSOSRes = InitialSOSRes
  { success :: Bool,
    trackingId :: Maybe Text,
    errorMessage :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON InitialSOSRes

instance FromJSON InitialSOSRes

instance ToSchema InitialSOSRes

-- | Unified SOS Trace Request (location updates)
data SOSTraceReq = SOSTraceReq
  { trackingId :: Text,
    dateTime :: Text,
    latitude :: Double,
    longitude :: Double,
    speed :: Maybe Double,
    gpsAccuracy :: Maybe Double
  }
  deriving (Show, Eq, Generic)

instance ToJSON SOSTraceReq

instance FromJSON SOSTraceReq

instance ToSchema SOSTraceReq

-- | Unified SOS Trace Response
data SOSTraceRes = SOSTraceRes
  { success :: Bool,
    errorMessage :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SOSTraceRes

instance FromJSON SOSTraceRes

instance ToSchema SOSTraceRes

-- | Unified Status Update Request
data SOSStatusUpdateReq = SOSStatusUpdateReq
  { trackingId :: Text,
    status :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SOSStatusUpdateReq

instance FromJSON SOSStatusUpdateReq

instance ToSchema SOSStatusUpdateReq

-- | Unified Status Update Response
data SOSStatusUpdateRes = SOSStatusUpdateRes
  { success :: Bool,
    errorMessage :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SOSStatusUpdateRes

instance FromJSON SOSStatusUpdateRes

instance ToSchema SOSStatusUpdateRes
