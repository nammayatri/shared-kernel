{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Verification.HyperVerge.Types
  ( module Reexport,
    module Kernel.External.Verification.HyperVerge.Types,
  )
where

import Data.Aeson
import Kernel.External.Common.HyperVerge.HyperVergeConfig as Reexport
import Kernel.Prelude hiding (error)

data HyperVergeRCValidationReq = HyperVergeRCValidationReq
  { rc :: Text,
    consent :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON HyperVergeRCValidationReq where
  toJSON HyperVergeRCValidationReq {..} =
    object
      [ "RC" .= rc,
        "consent" .= consent
      ]

instance FromJSON HyperVergeRCValidationReq where
  parseJSON = withObject "HyperVergeRCValidationReq" \o ->
    HyperVergeRCValidationReq
      <$> o .: "RC"
      <*> o .: "consent"

data HyperVergeRCValidationResp = HyperVergeRCValidationResp
  { status :: Text,
    statusCode :: Text,
    result :: Maybe ResultData,
    error :: Maybe (Either Text Err),
    message :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data ResultData = ResultData
  { transaction_id :: Text,
    resData :: Data,
    timestamp :: Integer
  }
  deriving (Show, Eq, Generic)

data Data = Data
  { code :: Text,
    message :: Text,
    rcData :: Maybe RCData
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance ToJSON ResultData where
  toJSON ResultData {..} =
    object
      [ "transaction_id" .= transaction_id,
        "data" .= resData,
        "timestamp" .= timestamp
      ]

instance FromJSON ResultData where
  parseJSON = withObject "ResultData" \o ->
    ResultData
      <$> o .: "transaction_id"
      <*> o .: "data"
      <*> o .: "timestamp"

data RCData = RCData
  { tax_end_date :: Maybe Text,
    financed :: Maybe Bool,
    permit_data :: Maybe PermitData,
    documentType :: Maybe Text,
    ownerData :: Maybe OwnerData,
    issueDate :: Maybe Text,
    expiryDate :: Maybe Text,
    registeredAt :: Maybe Text,
    insuranceData :: Maybe InsuranceData,
    vehicleData :: Maybe VehicleData,
    normsType :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data PermitData = PermitData
  { permit_number :: Text,
    permit_type :: Text,
    issue_date :: Text,
    expiry_date :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON PermitData where
  toJSON PermitData {..} =
    object
      [ "permit_number" .= permit_number,
        "type" .= permit_type,
        "issue_date" .= issue_date,
        "expiry_date" .= expiry_date
      ]

instance FromJSON PermitData where
  parseJSON = withObject "PermitData" \o ->
    PermitData
      <$> o .: "permit_number"
      <*> o .: "type"
      <*> o .: "issue_date"
      <*> o .: "expiry_date"

data OwnerData = OwnerData
  { serial :: Text,
    name :: Text,
    fatherName :: Text,
    presentAddress :: Text,
    permanentAddress :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data InsuranceData = InsuranceData
  { company :: Text,
    policyNumber :: Text,
    expiryDate :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data VehicleData = VehicleData
  { category :: Text,
    color :: Text,
    wheelbase :: Text,
    manufacturedDate :: Text,
    categoryDescription :: Text,
    chassisNumber :: Text,
    engineNumber :: Text,
    makerDescription :: Text,
    makerModel :: Text,
    bodyType :: Text,
    fuelType :: Text,
    cubicCapacity :: Text,
    grossWeight :: Text,
    numberOfCylinders :: Text,
    seatingCapacity :: Text,
    sleeperCapacity :: Text,
    unladenWeight :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Err = Err
  { transaction_id :: Text,
    error :: IntErr,
    timestamp :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data IntErr = IntErr
  { code :: Text,
    message :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
