{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Storage.Beam.SystemConfigs where

-- import qualified Data.Aeson as A
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

-- import qualified Data.Text as T

data SystemConfigsT f = SystemConfigsT
  { id :: B.C f Text,
    configValue :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table SystemConfigsT where
  data PrimaryKey SystemConfigsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SystemConfigs = SystemConfigsT Identity

-- instance HasSchemaName SystemConfigsT where
--   schemaName _ = T.pack "dynamic_driver_offer_db"

$(enableKVPG ''SystemConfigsT ['id] [])

$(mkTableInstancesGenericSchema ''SystemConfigsT "system_configs")
