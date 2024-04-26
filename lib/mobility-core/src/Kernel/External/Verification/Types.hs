{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Verification.Types
  ( module Kernel.External.Verification.Types,
  )
where

import Data.Aeson as A
import Data.OpenApi
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto (derivePersistField)

data VerificationService = Idfy | InternalScripts | GovtData | HyperVerge
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data DriverBackgroundVerificationService = SafetyPortal
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''VerificationService)
$(mkBeamInstancesForEnumAndList ''DriverBackgroundVerificationService)

availableVerificationServices :: [VerificationService]
availableVerificationServices = [Idfy, InternalScripts]

availableDriverBackgroundVerificationServices :: [DriverBackgroundVerificationService]
availableDriverBackgroundVerificationServices = [SafetyPortal]

derivePersistField "VerificationService"
derivePersistField "DriverBackgroundVerificationService"

data RCVerificationResponse = RCVerificationResponse
  { registrationDate :: Maybe Text,
    registrationNumber :: Maybe Text,
    fitnessUpto :: Maybe Text,
    insuranceValidity :: Maybe Text,
    vehicleClass :: Maybe Text,
    vehicleCategory :: Maybe Text,
    seatingCapacity :: Maybe A.Value,
    manufacturer :: Maybe Text,
    permitValidityFrom :: Maybe Text,
    permitValidityUpto :: Maybe Text,
    pucValidityUpto :: Maybe Text,
    manufacturerModel :: Maybe Text,
    mYManufacturing :: Maybe Text,
    colour :: Maybe Text,
    color :: Maybe Text,
    fuelType :: Maybe Text,
    bodyType :: Maybe Text,
    status :: Maybe Text
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)
