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

-- | Handler pattern for SOS services (like SMS handler)
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
-- Each provider's adapter picks the fields it needs; unused fields are ignored.
data InitialSOSReq = InitialSOSReq
  { -- | Optional source-generated ID
    sosId :: Maybe Text,
    -- | "YYYY-MM-DD HH:MM:SS"
    dateTime :: Text,
    -- | User's current latitude
    latitude :: Double,
    -- | User's current longitude
    longitude :: Double,
    -- | Speed if available
    speed :: Maybe Double,
    -- | User's mobile number (mandatory)
    mobileNo :: Text,
    -- | Device IMEI
    imeiNo :: Maybe Text,
    -- | GPS provider name
    gpsProvider :: Maybe Text,
    -- | User's name
    senderName :: Maybe Text,
    -- | User's address
    address :: Maybe Text,
    -- | GPS accuracy in meters
    gpsAccuracy :: Maybe Double,
    -- | State code
    stateCode :: Maybe Text,
    -- | Can operator call user?
    silentCommunication :: Maybe Bool,
    -- | Special assistance requirements
    specialNeeds :: Maybe Text,
    -- | Date of birth "YYYY-MM-DD"
    dob :: Maybe Text,
    -- | "MALE"/"FEMALE"/"OTHERS"
    gender :: Maybe Text,
    -- | Media file name or URL (ERSS)
    -- Ride context fields (used by GJ112 and similar providers)
    attachmentFileName :: Maybe Text,
    -- | Driver name
    driverName :: Maybe Text,
    -- | Driver phone number
    driverContactNo :: Maybe Text,
    -- | Vehicle registration/license plate
    vehicleNo :: Maybe Text,
    -- | Vehicle model
    vehicleModel :: Maybe Text,
    -- | Vehicle exterior color
    vehicleColor :: Maybe Text,
    -- | Vehicle category/type
    vehicleType :: Maybe Text,
    -- | Manufacturer/brand
    vehicleMake :: Maybe Text,
    -- | Distinctive visual markers
    vehicleAppearanceNotes :: Maybe Text,
    -- | Vehicle latitude
    vehicleLat :: Maybe Double,
    -- | Vehicle longitude
    vehicleLon :: Maybe Double,
    -- | Vehicle location tracking URL
    vehicleLocationUrl :: Maybe Text,
    -- | Video/telemetry tracking URL
    -- Emergency contact fields
    videoPath :: Maybe Text,
    -- | Primary emergency contact name
    emergencyContact1Name :: Maybe Text,
    -- | Primary emergency contact phone
    emergencyContact1Phone :: Maybe Text,
    -- | Secondary emergency contact name
    emergencyContact2Name :: Maybe Text,
    -- | Secondary emergency contact phone
    -- Additional context
    emergencyContact2Phone :: Maybe Text,
    -- | City of event/user
    city :: Maybe Text,
    -- | Context for SOS trigger
    emergencyMessage :: Maybe Text,
    -- | User email
    email :: Maybe Text,
    -- | Provider/platform/vendor name
    vendorName :: Maybe Text,
    -- | Device type enum (client-defined)
    deviceType :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON InitialSOSReq

instance FromJSON InitialSOSReq

instance ToSchema InitialSOSReq

-- | Unified Initial SOS Response
data InitialSOSRes = InitialSOSRes
  { success :: Bool,
    -- | Tracking ID for subsequent calls
    trackingId :: Maybe Text,
    errorMessage :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON InitialSOSRes

instance FromJSON InitialSOSRes

instance ToSchema InitialSOSRes

-- | Unified SOS Trace Request (location updates)
data SOSTraceReq = SOSTraceReq
  { -- | From Initial SOS response
    trackingId :: Text,
    -- | "YYYY-MM-DD HH:MM:SS"
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
