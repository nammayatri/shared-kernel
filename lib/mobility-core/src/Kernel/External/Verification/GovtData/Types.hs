{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.External.Verification.GovtData.Types where

import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id, state)

data GovtDataResponse = GovtDataResponse
  { id :: Text,
    merchantOperatingCityId :: Text,
    ownerSerialNumber :: Maybe Text,
    registrationNumber :: Maybe Text,
    manufacturerModel :: Maybe Text,
    permitValidityFrom :: Maybe Text,
    permitValidityUpto :: Maybe Text,
    manufacturer :: Maybe Text,
    bodyType :: Maybe Text,
    numberOfCylinder :: Maybe Int,
    fuelType :: Maybe Text,
    seatingCapacity :: Maybe Int,
    fromDate :: Maybe Text,
    toDate :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)
