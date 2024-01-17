{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Verification.GovtData.Storage.Beam where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH (enableKVPG, mkTableInstancesGenericSchema)
import Kernel.Prelude

data GovtDataRCT f = GovtDataRCT
  { id :: B.C f Text,
    merchantOperatingCityId :: B.C f Text,
    ownerSerialNumber :: B.C f (Maybe Text),
    registrationNumber :: B.C f (Maybe Text),
    manufacturerModel :: B.C f (Maybe Text),
    permitValidityFrom :: B.C f (Maybe Text),
    permitValidityUpto :: B.C f (Maybe Text),
    manufacturer :: B.C f (Maybe Text),
    bodyType :: B.C f (Maybe Text),
    numberOfCylinder :: B.C f (Maybe Int),
    fuelType :: B.C f (Maybe Text),
    seatingCapacity :: B.C f (Maybe Int),
    fromDate :: B.C f (Maybe Text),
    toDate :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table GovtDataRCT where
  data PrimaryKey GovtDataRCT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type GovtDataRC = GovtDataRCT Identity

$(enableKVPG ''GovtDataRCT ['id] [])

$(mkTableInstancesGenericSchema ''GovtDataRCT "govt_data_r_c")
