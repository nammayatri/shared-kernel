{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

 distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

 FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

 General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Verification.Tten.Types where

import Data.Aeson
import Kernel.Prelude

data ApplicationDetailsReq = ApplicationDetailsReq
  { tten_no :: Text,
    mobile_no :: Text,
    udin_no :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ApplicationDetailsResp = ApplicationDetailsResp
  { version :: Maybe Text,
    status :: Maybe Int,
    message :: Maybe Text,
    data_ :: [TtenDetails]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ApplicationDetailsResp where
  parseJSON = genericParseJSON jsonOptionsApplicationDetailsResp

instance ToJSON ApplicationDetailsResp where
  toJSON = genericToJSON jsonOptionsApplicationDetailsResp

data TtenDetails = TtenDetails
  { user_full_name :: Maybe Text,
    mobile_number :: Maybe Text,
    date_of_birth :: Maybe Text,
    fathers_name :: Maybe Text,
    gender :: Maybe Text,
    address :: Maybe Text,
    user_photo_file_path :: Maybe Text,
    blood_group_name :: Maybe Text,
    district_name :: Maybe Text,
    police_district_name :: Maybe Text,
    police_station_name :: Maybe Text,
    rtoauthorization :: Maybe Text,
    issue_date :: Maybe Text,
    validity :: Maybe Text,
    verified_by :: Maybe Text,
    verification_office :: Maybe Text,
    rto_name :: Maybe Text,
    zone_name :: Maybe Text,
    vehicle_make :: Maybe Text,
    vehicle_model :: Maybe Text,
    chasis_number :: Maybe Text,
    motor_number :: Maybe Text,
    motor_power :: Maybe Double,
    vehicle_colour :: Maybe Text,
    wbsedcl_meter_number :: Maybe Text,
    is_self_driven :: Maybe Int,
    photo_of_vehicle_path :: Maybe Text,
    name_of_driver :: Maybe Text,
    driver_mobile_number :: Maybe Text,
    driver_dl_number :: Maybe Text,
    driver_photo :: Maybe Text,
    dealer_name :: Maybe Text,
    dealer_address :: Maybe Text,
    invoice_number :: Maybe Text,
    reference_document_file_path :: Maybe Text,
    is_disclaimer_accepted :: Maybe Text,
    amount :: Maybe Text,
    udin_number :: Maybe Text,
    tten_number :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

jsonOptionsApplicationDetailsResp :: Options
jsonOptionsApplicationDetailsResp =
  defaultOptions
    { fieldLabelModifier = \case
        "data_" -> "data"
        other -> other
    }

data TtenVerificationCfg = TtenVerificationCfg
  { url :: BaseUrl
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
