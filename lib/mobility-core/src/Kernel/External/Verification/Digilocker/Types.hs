{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}

module Kernel.External.Verification.Digilocker.Types where

import Kernel.Prelude
import Web.FormUrlEncoded (ToForm (..))

data DigiLockerCfg = DigiLockerCfg
  { url :: BaseUrl,
    clientId :: Text,
    clientSecret :: Text,
    redirectUri :: Text,
    codeChallengeMethod :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Pull Driving License Document API types
data DigiLockerPullDrivingLicenseRequest = DigiLockerPullDrivingLicenseRequest
  { orgid :: Text,
    doctype :: Text,
    consent :: Text, -- "Y" or "N"
    dlno :: Text -- Driving License Number
  }
  deriving stock (Show, Eq, Generic)

instance ToForm DigiLockerPullDrivingLicenseRequest where
  toForm DigiLockerPullDrivingLicenseRequest {..} =
    [ ("orgid", orgid),
      ("doctype", doctype),
      ("consent", consent),
      ("dlno", dlno)
    ]

data DigiLockerPullDocumentResponse = DigiLockerPullDocumentResponse
  { uri :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- DigiLocker verified extraction flow types (similar structure to HyperVerge but DigiLocker-specific)
data DigiLockerPanFlow = DigiLockerPanFlow
  { pan :: Maybe Text,
    name :: Maybe Text,
    dob :: Maybe Text,
    gender :: Maybe Text,
    panURL :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DigiLockerAadhaarFlow = DigiLockerAadhaarFlow
  { idNumber :: Maybe Text,
    fullName :: Maybe Text,
    dob :: Maybe Text,
    address :: Maybe Text,
    city :: Maybe Text,
    pincode :: Maybe Text,
    aadhaarFrontURL :: Maybe Text,
    aadhaarBackURL :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DigiLockerDLFlow = DigiLockerDLFlow
  { dlNumber :: Maybe Text,
    name :: Maybe Text,
    dob :: Maybe Text,
    dlURL :: Maybe Text,
    expiryDate :: Maybe Text,
    classOfVehicles :: Maybe [Text],
    dateOfIssue :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- DigiLocker request types
-- DigiLocker DL extraction request
data DigiLockerExtractDLReq = DigiLockerExtractDLReq
  { uri :: Text,
    accessToken :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- DigiLocker PAN extraction request
data DigiLockerExtractPanReq = DigiLockerExtractPanReq
  { accessToken :: Text,
    uri :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- DigiLocker Aadhaar extraction request
data DigiLockerExtractAadhaarReq = DigiLockerExtractAadhaarReq
  { accessToken :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DigiLockerGetFileReq = DigiLockerGetFileReq
  { uri :: Text,
    accessToken :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DigiLockerPullDrivingLicenseReq = DigiLockerPullDrivingLicenseReq
  { accessToken :: Text,
    orgid :: Text,
    doctype :: Text,
    consent :: Text,
    dlno :: Text -- Driving License Number
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
