{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.External.Verification.GovtData.Storage.ClickHouse where

import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH hiding (toDate)
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH

data GovtDataT f = GovtDataT
  { id :: C f Text,
    merchantOperatingCityId :: C f Text,
    ownerSerialNumber :: C f (Maybe Text),
    registrationNumber :: C f (Maybe Text),
    manufacturerModel :: C f (Maybe Text),
    permitValidityFrom :: C f (Maybe Text),
    permitValidityUpto :: C f (Maybe Text),
    manufacturer :: C f (Maybe Text),
    bodyType :: C f (Maybe Text),
    numberOfCylinder :: C f (Maybe Int),
    fuelType :: C f (Maybe Text),
    seatingCapacity :: C f (Maybe Int),
    fromDate :: C f (Maybe Text),
    toDate :: C f (Maybe Text),
    createdAt :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show GovtData

govtDataTTable :: GovtDataT (FieldModification GovtDataT)
govtDataTTable =
  GovtDataT
    { id = "id",
      merchantOperatingCityId = "merchant_operating_city_id",
      ownerSerialNumber = "owner_serial_number",
      registrationNumber = "registration_number",
      manufacturerModel = "manufacturer_model",
      permitValidityFrom = "permit_validity_from",
      permitValidityUpto = "permit_validity_upto",
      manufacturer = "manufacturer",
      bodyType = "body_type",
      numberOfCylinder = "number_of_cylinder",
      fuelType = "fuel_type",
      seatingCapacity = "seating_capacity",
      fromDate = "from_date",
      toDate = "to_date",
      createdAt = "created_at"
    }

type GovtData = GovtDataT Identity

$(TH.mkClickhouseInstances ''GovtDataT)

findByRCNumber ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  m (Maybe (GovtDataT Identity))
findByRCNumber rcNumber = do
  govtData <-
    CH.findAll $
      CH.select $
        CH.filter_
          ( \govtData _ ->
              govtData.registrationNumber CH.==. (Just rcNumber)
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE govtDataTTable)
  return $ listToMaybe govtData
