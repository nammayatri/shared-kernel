{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

 distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

 FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

 General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Verification.Morth.Types where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude

-- | Configuration for the MoRTH (Ministry of Road, Transport and Highways)
-- Parivahan verification service.
data MorthVerificationCfg = MorthVerificationCfg
  { url :: BaseUrl,
    -- | API key passed as the @X-API-Key@ header
    apiKey :: EncryptedField 'AsEncrypted Text,
    applicantMobile :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Vehicle Registration (RC) validity API
-- POST /vehicle/getVehicleValidityInfo
-- ---------------------------------------------------------------------------

data VehicleValidityReq = VehicleValidityReq
  { regnNo :: Text,
    applicantMobile :: Text,
    engNo :: Text,
    chasiNo :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data VehicleValidityData = VehicleValidityData
  { rcValidity :: Maybe Text,
    rcValidUpto :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data VehicleValidityResp = VehicleValidityResp
  { success :: Bool,
    message :: Maybe Text,
    statusCode :: Int,
    data_ :: Maybe VehicleValidityData
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON VehicleValidityResp where
  parseJSON = genericParseJSON vehicleValidityRespOptions

instance ToJSON VehicleValidityResp where
  toJSON = genericToJSON vehicleValidityRespOptions

vehicleValidityRespOptions :: Options
vehicleValidityRespOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "data_" -> "data"
        other -> other
    }

-- ---------------------------------------------------------------------------
-- Vehicle basic info API
-- POST /vehicle/getVehicleBasicInfo
-- ---------------------------------------------------------------------------

data VehicleBasicInfoReq = VehicleBasicInfoReq
  { regnNo :: Text,
    applicantMobile :: Text,
    engNo :: Text,
    chasiNo :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Response data for getVehicleBasicInfo. Note: API uses "vehcile" typo.
data VehicleBasicInfoData = VehicleBasicInfoData
  { vehcileClass :: Maybe Text,
    vehcileCatg :: Maybe Text,
    maker :: Maybe Text,
    model :: Maybe Text,
    color :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data VehicleBasicInfoResp = VehicleBasicInfoResp
  { success :: Bool,
    message :: Maybe Text,
    statusCode :: Int,
    data_ :: Maybe VehicleBasicInfoData
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON VehicleBasicInfoResp where
  parseJSON = genericParseJSON vehicleBasicInfoRespOptions

instance ToJSON VehicleBasicInfoResp where
  toJSON = genericToJSON vehicleBasicInfoRespOptions

vehicleBasicInfoRespOptions :: Options
vehicleBasicInfoRespOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "data_" -> "data"
        other -> other
    }

-- ---------------------------------------------------------------------------
-- Driving License validity API
-- POST /dl/getDrivinglicenseValidityInfo
-- ---------------------------------------------------------------------------

data DrivingLicenseValidityReq = DrivingLicenseValidityReq
  { drivingLicense :: Text,
    applicantMobile :: Text,
    dob :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DrivingLicenseValidityData = DrivingLicenseValidityData
  { dlvalidity :: Maybe Text,
    dlUpto :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DrivingLicenseValidityResp = DrivingLicenseValidityResp
  { success :: Bool,
    message :: Maybe Text,
    statusCode :: Int,
    data_ :: Maybe DrivingLicenseValidityData
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON DrivingLicenseValidityResp where
  parseJSON = genericParseJSON drivingLicenseValidityRespOptions

instance ToJSON DrivingLicenseValidityResp where
  toJSON = genericToJSON drivingLicenseValidityRespOptions

drivingLicenseValidityRespOptions :: Options
drivingLicenseValidityRespOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "data_" -> "data"
        other -> other
    }

-- ---------------------------------------------------------------------------
-- Driving License class-wise validity API
-- POST /dl/getDrivinglicenseClassWiseValidity
-- ---------------------------------------------------------------------------

data DrivingLicenseClassWiseValidityReq = DrivingLicenseClassWiseValidityReq
  { drivingLicense :: Text,
    applicantMobile :: Text,
    dob :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DrivingLicenseClassWiseValidityData = DrivingLicenseClassWiseValidityData
  { dlvehicleClass :: Maybe Text,
    dlvalidityDate :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DrivingLicenseClassWiseValidityResp = DrivingLicenseClassWiseValidityResp
  { success :: Bool,
    message :: Maybe Text,
    statusCode :: Int,
    data_ :: Maybe [DrivingLicenseClassWiseValidityData]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON DrivingLicenseClassWiseValidityResp where
  parseJSON = genericParseJSON drivingLicenseClassWiseValidityRespOptions

instance ToJSON DrivingLicenseClassWiseValidityResp where
  toJSON = genericToJSON drivingLicenseClassWiseValidityRespOptions

drivingLicenseClassWiseValidityRespOptions :: Options
drivingLicenseClassWiseValidityRespOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "data_" -> "data"
        other -> other
    }
