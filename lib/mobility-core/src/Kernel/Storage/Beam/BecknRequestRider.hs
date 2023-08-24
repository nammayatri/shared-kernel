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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Storage.Beam.BecknRequestRider where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

data BecknRequestT f = BecknRequestT
  { id :: B.C f Text,
    becknRequest :: B.C f Text,
    signatureHeader :: B.C f Text,
    timeStamp :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BecknRequestT where
  data PrimaryKey BecknRequestT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type BecknRequest = BecknRequestT Identity

becknRequestTMod :: BecknRequestT (B.FieldModification (B.TableField BecknRequestT))
becknRequestTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      becknRequest = B.fieldNamed "beckn_request",
      signatureHeader = B.fieldNamed "signature_header",
      timeStamp = B.fieldNamed "time_stamp"
    }

$(enableKVPG ''BecknRequestT ['id] [])

$(mkTableInstances ''BecknRequestT "beckn_request" "atlas_app")
