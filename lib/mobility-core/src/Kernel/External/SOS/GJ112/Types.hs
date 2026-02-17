{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.SOS.GJ112.Types where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Servant.Client (ResponseF (responseBody))
import Web.Internal.HttpApiData (ToHttpApiData (..))

-- | Authentication request (userName and password are Base64-encoded)
data GJ112AuthReq = GJ112AuthReq
  { -- | Base64-encoded username
    userName :: Text,
    -- | Base64-encoded password
    password :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON GJ112AuthReq

instance FromJSON GJ112AuthReq

-- | Authentication response from GJ112
data GJ112AuthRes = GJ112AuthRes
  { -- | Full token string (already includes "Bearer")
    token :: Text,
    -- | Token creation time in milliseconds since epoch
    tokenCreationTime :: Integer,
    -- | Token TTL in seconds
    expiresAt :: Int,
    -- | Role of authenticated identity
    employeeRole :: Text,
    -- | Name/label of authenticated identity
    employeeName :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON GJ112AuthRes

instance ToJSON GJ112AuthRes

-- | SOS Event Creation Request matching GJ112 API spec
data GJ112SOSReq = GJ112SOSReq
  { -- | Unique client identifier (required)
    clientId :: Text,
    -- | Client short code/label (required)
    clientCode :: Text,
    -- | User's name at event time (required)
    name :: Text,
    -- | City of event/user (required)
    city :: Text,
    -- | Address or area text
    address :: Maybe Text,
    -- | Device unique identifier
    deviceUuid :: Maybe Text,
    -- | Email for notifications/case updates
    email :: Maybe Text,
    -- | Primary emergency contact name
    relativeName1 :: Maybe Text,
    -- | Secondary emergency contact name
    relativeName2 :: Maybe Text,
    -- | Primary emergency contact phone
    relativeContact1 :: Maybe Text,
    -- | Secondary emergency contact phone
    relativeContact2 :: Maybe Text,
    -- | Gender label (MALE/FEMALE/OTHER)
    gender :: Maybe Text,
    -- | SIM/MSISDN associated with device
    simNo :: Maybe Text,
    -- | Event timestamp (required)
    datetime :: Text,
    -- | Context for SOS trigger (required)
    emergencyMessage :: Text,
    -- | User latitude (required)
    latitude :: Text,
    -- | User longitude (required)
    longitude :: Text,
    -- | Video/telemetry tracking URL
    videoPath :: Maybe Text,
    -- | Driver name (ride-hailing context)
    driverName :: Maybe Text,
    -- | Driver phone
    driverContactNo :: Maybe Text,
    -- | Vehicle registration/license plate
    vehicleNo :: Maybe Text,
    -- | Vehicle model
    vehicleModel :: Maybe Text,
    -- | Vehicle latitude
    vehLat :: Maybe Text,
    -- | Vehicle longitude
    vehLng :: Maybe Text,
    -- | Device type enum (client-defined)
    deviceType :: Maybe Int,
    -- | Vehicle location tracking URL
    vehLocUrl :: Maybe Text,
    -- | Provider/platform/vendor name
    vendorName :: Maybe Text,
    -- | Vehicle exterior color
    vehicleColor :: Maybe Text,
    -- | Vehicle category/type
    vehicleType :: Maybe Text,
    -- | Manufacturer/brand
    vehicleMake :: Maybe Text,
    -- | Distinctive visual markers
    vehicleAppearanceNotes :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON GJ112SOSReq

instance FromJSON GJ112SOSReq

instance ToSchema GJ112SOSReq

-- | SOS Event Creation Response
data GJ112SOSRes = GJ112SOSRes
  { -- | Response message
    message :: Maybe Text,
    -- | HTTP-like response code
    responseCode :: Maybe Int,
    -- | Reference ID for the SOS event
    referenceId :: Maybe Integer,
    -- | Action taken (e.g. "Dispatch notified")
    action :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON GJ112SOSRes

instance ToJSON GJ112SOSRes

instance ToSchema GJ112SOSRes

-- | GJ112 API Error
newtype GJ112Error = GJ112Error Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''GJ112Error

instance IsBaseError GJ112Error where
  toMessage (GJ112Error msg) = Just $ "GJ112 Error: " <> msg

instance IsHTTPError GJ112Error where
  toErrorCode (GJ112Error _) = "GJ112_ERROR"
  toHttpCode (GJ112Error _) = E500

instance IsAPIError GJ112Error

instance FromResponse GJ112Error where
  fromResponse resp =
    case decode (responseBody resp) of
      Just err -> Just err
      Nothing -> Just $ GJ112Error "Unknown GJ112 API error"

instance FromJSON GJ112Error where
  parseJSON = withObject "GJ112Error" $ \o -> do
    errMsg <- o .:? "message" .!= "Unknown error"
    pure $ GJ112Error errMsg

instance ToJSON GJ112Error where
  toJSON (GJ112Error msg) = object ["message" .= msg]

-- | Auth token wrapper for Authorization header
-- Note: GJ112 token already includes "Bearer " prefix
newtype GJ112AuthToken = GJ112AuthToken Text
  deriving (Show, Eq, Generic)

instance ToHttpApiData GJ112AuthToken where
  toUrlPiece (GJ112AuthToken token) = token
