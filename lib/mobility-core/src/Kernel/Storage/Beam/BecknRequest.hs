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

module Kernel.Storage.Beam.BecknRequest where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data BecknRequestT f = BecknRequestT
  { id :: B.C f Text,
    becknRequest :: B.C f Text,
    signatureHeader :: B.C f Text,
    timeStamp :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BecknRequestT where
  data PrimaryKey BecknRequestT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type BecknRequest = BecknRequestT Identity

$(enableKVPG ''BecknRequestT ['id] [])

$(mkTableInstancesGenericSchema ''BecknRequestT "beckn_request")

-- How it works inside of template:
-- instance HasSchemaName BecknRequestT => Sequelize.ModelMeta BecknRequestT where
--   modelFieldModification = becknRequestTMod
--   modelTableName = "booking"
--   modelSchemaName = Just (schemaName (Proxy @BecknRequestT))

-- How it works inside of template:
-- becknRequestTable :: HasSchemaName BecknRequestT => B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity BecknRequestT)
-- becknRequestTable = B.setEntitySchema (Just (schemaName (Proxy @BecknRequestT))) <> (B.setEntityName "beckn_request" <> B.modifyTableFields becknRequestTMod)
