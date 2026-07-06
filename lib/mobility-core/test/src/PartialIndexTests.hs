{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests for partial (pinned) secondary keys via 'enableKVPGWithPartialIndex'.
--
--   The table has a normal secondary key on driverId and a partial one on status pinned to "ACTIVE".
--   We check that secondaryKeys only emits the pinned key when status is ACTIVE - that is what the KV
--   layer uses to decide index membership on create/find/update.
module PartialIndexTests (partialIndexTests) where

import qualified Data.HashMap.Strict as HM
import qualified Database.Beam as B
import EulerHS.KVConnector.Types (KVConnector (keyMap, secondaryKeys), SecondaryKey (..))
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Lib.UtilsTH
import Test.Tasty
import Test.Tasty.HUnit

data PartialTestT f = PartialTestT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    status :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table PartialTestT where
  data PrimaryKey PartialTestT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type PartialTest = PartialTestT Identity

$( enableKVPGWithPartialIndex
     ''PartialTestT
     ['id]
     [['driverId]] -- plain secondary keys (same syntax as enableKVPG)
     [SKeyPartial [('status, "ACTIVE")]] -- partial: status indexed only when ACTIVE
 )

row :: Text -> PartialTest
row st = PartialTestT {id = "id-" <> st, driverId = "d1", status = st}

-- | The raw (field, value) pairs of each secondary key the row currently carries.
sKeys :: PartialTest -> [[(Text, Text)]]
sKeys = map (\(SKey s) -> s) . secondaryKeys

partialIndexTests :: TestTree
partialIndexTests =
  testGroup
    "KV partial index (enableKVPGWithPartialIndex)"
    [ testCase "ACTIVE row IS indexed under the pinned status key" $
        sKeys (row "ACTIVE") @?= [[("driverId", "d1")], [("status", "ACTIVE")]],
      testCase "non-ACTIVE row is NOT indexed under the pinned status key" $
        sKeys (row "INACTIVE") @?= [[("driverId", "d1")]],
      testCase "another non-ACTIVE value is also skipped" $
        sKeys (row "COMPLETED") @?= [[("driverId", "d1")]],
      testCase "keyMap registers id (pk) + driverId + status" $
        sort (HM.keys (keyMap @PartialTest)) @?= sort ["id", "driverId", "status"]
    ]
