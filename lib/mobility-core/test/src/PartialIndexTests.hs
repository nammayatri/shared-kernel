{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests for partial (pinned) secondary keys via 'enableKVPGWithPartialIndex'.
--
--   Write side: 'secondaryKeys' emits the pinned key only when the row's field equals one of its
--   pinned values - that decides redis set membership on create/update.
--
--   Read side: 'pinnedKeyMap' publishes the pins, and 'validKeyMapForClause' (euler-hs) restricts
--   a where-clause combination to the keys it may trust. A pinned set is empty for every
--   non-pinned value WITHOUT meaning "no rows", so a pinned key must never intersect with other
--   keys: it is usable only as the sole key in the clause, and only for a pinned value.
--   (Regression test for the driver dues incident, 2026-07: driverId results were intersected
--   with the always-empty serviceName/feeType sets and every dues query lost its rows.)
module PartialIndexTests (partialIndexTests) where

import qualified Data.HashMap.Strict as HM
import Data.List ((\\))
import qualified Database.Beam as B
import EulerHS.KVConnector.Types (KVConnector (keyMap, pinnedKeyMap, secondaryKeys), SecondaryKey (..))
import EulerHS.KVConnector.Utils (validKeyMapForClause)
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

-- | The same field pinned to two values: 'groupPins' must collapse them to ONE key that is
--   written when the row's value is either of them (OR, not an unsatisfiable AND).
data MultiPinTestT f = MultiPinTestT
  { mpId :: B.C f Text,
    mpStatus :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table MultiPinTestT where
  data PrimaryKey MultiPinTestT f
    = MpId (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = MpId . mpId

type MultiPinTest = MultiPinTestT Identity

$( enableKVPGWithPartialIndex
     ''MultiPinTestT
     ['mpId]
     []
     [SKeyPartial [('mpStatus, "NEW"), ('mpStatus, "PENDING")]]
 )

-- | A field with BOTH a plain skey and a pin: the plain spec keeps the set complete for every
--   value, so the field stays a full regular key - the pin must not restrict reads
--   (Invoice.invoiceStatus / Notification.status shape).
data PlainPrecTestT f = PlainPrecTestT
  { ppId :: B.C f Text,
    ppStatus :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table PlainPrecTestT where
  data PrimaryKey PlainPrecTestT f
    = PpId (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = PpId . ppId

type PlainPrecTest = PlainPrecTestT Identity

$( enableKVPGWithPartialIndex
     ''PlainPrecTestT
     ['ppId]
     [['ppStatus]]
     [SKeyPartial [('ppStatus, "ACTIVE")]]
 )

-- | The same field pinned via two SEPARATE SKeyPartial specs: values must merge in pinnedKeyMap
--   (HM.fromList is last-wins - unmerged duplicates would drop the first spec's values from
--   read-path eligibility while the write side still maintains both sets).
data SplitPinTestT f = SplitPinTestT
  { spId :: B.C f Text,
    spStatus :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table SplitPinTestT where
  data PrimaryKey SplitPinTestT f
    = SpId (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = SpId . spId

type SplitPinTest = SplitPinTestT Identity

$( enableKVPGWithPartialIndex
     ''SplitPinTestT
     ['spId]
     []
     [SKeyPartial [('spStatus, "ACTIVE")], SKeyPartial [('spStatus, "INACTIVE")]]
 )

-- | Mirror of the production DriverFee table: pkey id, plain skey driverId, and the two pins
--   that caused the 2026-07 dues incident. dfStatus is a normal column (status is deny-listed
--   as a key in production) - it must always resolve to in-app filtering.
data DriverFeeTestT f = DriverFeeTestT
  { dfId :: B.C f Text,
    dfDriverId :: B.C f Text,
    dfFeeType :: B.C f Text,
    dfServiceName :: B.C f Text,
    dfStatus :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverFeeTestT where
  data PrimaryKey DriverFeeTestT f
    = DfId (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = DfId . dfId

type DriverFeeTest = DriverFeeTestT Identity

$( enableKVPGWithPartialIndex
     ''DriverFeeTestT
     ['dfId]
     [['dfDriverId]]
     [SKeyPartial [('dfFeeType, "CANCELLATION_PENALTY")], SKeyPartial [('dfServiceName, "YATRI_RENTAL")]]
 )

row :: Text -> PartialTest
row st = PartialTestT {id = "id-" <> st, driverId = "d1", status = st}

-- | feeType -> serviceName -> a DriverFee-shaped row for driver d1.
dfRow :: Text -> Text -> DriverFeeTest
dfRow ft sn = DriverFeeTestT {dfId = "fee-1", dfDriverId = "d1", dfFeeType = ft, dfServiceName = sn, dfStatus = "PAYMENT_PENDING"}

-- | Which clause fields actually drive redis key lookups for one and-combination - mirrors
--   getPrimaryKeyFromFieldsAndValues: a field is consulted iff it is in the clause-valid keyMap.
--   find, findAll, update, updateAll and delete all locate rows through exactly this decision.
dfResolvesVia :: [(Text, Text)] -> [Text]
dfResolvesVia combo =
  sort [k | (k, _) <- combo, HM.member k (validKeyMapForClause (pinnedKeyMap @DriverFeeTest) (keyMap @DriverFeeTest) combo)]

-- | Set memberships an update transition adds/removes - mirrors modifySKeysRedis, which diffs
--   secondaryKeys of the old row against the new row and SADDs/SREMs accordingly.
dfSetsJoined :: DriverFeeTest -> DriverFeeTest -> [[(Text, Text)]]
dfSetsJoined old new = sKeysOf new \\ sKeysOf old

dfSetsLeft :: DriverFeeTest -> DriverFeeTest -> [[(Text, Text)]]
dfSetsLeft old new = sKeysOf old \\ sKeysOf new

spRow :: Text -> SplitPinTest
spRow st = SplitPinTestT {spId = "id-" <> st, spStatus = st}

mpRow :: Text -> MultiPinTest
mpRow st = MultiPinTestT {mpId = "id-" <> st, mpStatus = st}

-- | The raw (field, value) pairs of each secondary key the row currently carries.
sKeysOf :: KVConnector t => t -> [[(Text, Text)]]
sKeysOf = map (\(SKey s) -> s) . secondaryKeys

-- | Shorthand: the keyMap 'PartialTest' resolution may use for a given clause combination.
validFor :: [(Text, Text)] -> HM.HashMap Text Bool
validFor = validKeyMapForClause (pinnedKeyMap @PartialTest) (keyMap @PartialTest)

partialIndexTests :: TestTree
partialIndexTests =
  testGroup
    "KV partial index (enableKVPGWithPartialIndex)"
    [ testGroup
        "write side: secondaryKeys gating"
        [ testCase "ACTIVE row IS indexed under the pinned status key" $
            sKeysOf (row "ACTIVE") @?= [[("driverId", "d1")], [("status", "ACTIVE")]],
          testCase "non-ACTIVE row is NOT indexed under the pinned status key" $
            sKeysOf (row "INACTIVE") @?= [[("driverId", "d1")]],
          testCase "another non-ACTIVE value is also skipped" $
            sKeysOf (row "COMPLETED") @?= [[("driverId", "d1")]],
          testCase "multi-value pin: first pinned value is indexed" $
            sKeysOf (mpRow "NEW") @?= [[("mpStatus", "NEW")]],
          testCase "multi-value pin: second pinned value is indexed (OR, not AND)" $
            sKeysOf (mpRow "PENDING") @?= [[("mpStatus", "PENDING")]],
          testCase "multi-value pin: unpinned value is not indexed" $
            sKeysOf (mpRow "DONE") @?= [],
          testCase "split specs, same field: each spec's pinned value is indexed" $
            (sKeysOf (spRow "ACTIVE"), sKeysOf (spRow "INACTIVE")) @?= ([[("spStatus", "ACTIVE")]], [[("spStatus", "INACTIVE")]]),
          testCase "split specs, same field: unpinned value is not indexed" $
            sKeysOf (spRow "DONE") @?= []
        ],
      testGroup
        "generated metadata"
        [ testCase "keyMap registers id (pk) + driverId + status" $
            sort (HM.keys (keyMap @PartialTest)) @?= sort ["id", "driverId", "status"],
          testCase "pinnedKeyMap publishes the pin" $
            pinnedKeyMap @PartialTest @?= HM.fromList [("status", ["ACTIVE"])],
          testCase "multi-value pin collapses to one keyMap entry (no mpStatus_mpStatus junk)" $
            sort (HM.keys (keyMap @MultiPinTest)) @?= sort ["mpId", "mpStatus"],
          testCase "multi-value pin publishes both values" $
            pinnedKeyMap @MultiPinTest @?= HM.fromList [("mpStatus", ["NEW", "PENDING"])],
          testCase "plain skey precedence: pin on a plain-keyed field is not published" $
            pinnedKeyMap @PlainPrecTest @?= HM.empty,
          testCase "split specs, same field: pinned values merge instead of last-wins" $
            pinnedKeyMap @SplitPinTest @?= HM.fromList [("spStatus", ["ACTIVE", "INACTIVE"])],
          testCase "split specs, same field: single keyMap entry" $
            sort (HM.keys (keyMap @SplitPinTest)) @?= sort ["spId", "spStatus"]
        ],
      testGroup
        "read side: validKeyMapForClause"
        [ testCase "regular skey present: pinned field dropped even when value matches the pin" $
            validFor [("driverId", "d1"), ("status", "ACTIVE")]
              @?= HM.fromList [("id", True), ("driverId", False)],
          testCase "regular skey present, non-pinned value: pinned field dropped (dues regression)" $
            validFor [("driverId", "d1"), ("status", "INACTIVE")]
              @?= HM.fromList [("id", True), ("driverId", False)],
          testCase "primary key present: pinned field dropped" $
            validFor [("id", "x"), ("status", "ACTIVE")]
              @?= HM.fromList [("id", True), ("driverId", False)],
          testCase "pinned field is the sole key with a pinned value: usable" $
            validFor [("status", "ACTIVE")] @?= keyMap @PartialTest,
          testCase "pinned field is the sole key with a non-pinned value: dropped (empty set is not 'no rows')" $
            validFor [("status", "INACTIVE")]
              @?= HM.fromList [("id", True), ("driverId", False)],
          testCase "clause with no key fields at all: keyMap untouched (resolution finds no keys anyway)" $
            validFor [("someField", "someValue")] @?= keyMap @PartialTest,
          testCase "table without pins: keyMap always returned unchanged" $
            validKeyMapForClause (pinnedKeyMap @PlainPrecTest) (keyMap @PlainPrecTest) [("ppStatus", "ANYTHING")]
              @?= keyMap @PlainPrecTest
        ],
      testGroup
        "DriverFee shape: create"
        [ testCase "subscription fee: only the driverId set is SADDed" $
            sKeysOf (dfRow "RECURRING_INVOICE" "YATRI_SUBSCRIPTION") @?= [[("dfDriverId", "d1")]],
          testCase "cancellation penalty fee: joins the pinned feeType set too" $
            sKeysOf (dfRow "CANCELLATION_PENALTY" "YATRI_SUBSCRIPTION")
              @?= [[("dfDriverId", "d1")], [("dfFeeType", "CANCELLATION_PENALTY")]],
          testCase "rental fee: joins the pinned serviceName set too" $
            sKeysOf (dfRow "RECURRING_INVOICE" "YATRI_RENTAL")
              @?= [[("dfDriverId", "d1")], [("dfServiceName", "YATRI_RENTAL")]],
          testCase "rental cancellation penalty: joins both pinned sets" $
            sKeysOf (dfRow "CANCELLATION_PENALTY" "YATRI_RENTAL")
              @?= [[("dfDriverId", "d1")], [("dfFeeType", "CANCELLATION_PENALTY")], [("dfServiceName", "YATRI_RENTAL")]]
        ],
      testGroup
        "DriverFee shape: find / findAll (row location per and-combination)"
        [ testCase "findById: located via the primary key" $
            dfResolvesVia [("dfId", "fee-1")] @?= ["dfId"],
          testCase "dues query (driverId + status + serviceName + feeType): driverId only - the incident regression" $
            dfResolvesVia [("dfDriverId", "d1"), ("dfStatus", "PAYMENT_PENDING"), ("dfServiceName", "YATRI_SUBSCRIPTION"), ("dfFeeType", "RECURRING_INVOICE")]
              @?= ["dfDriverId"],
          testCase "feeType In [..] expands per value: non-pinned value combo uses driverId only" $
            dfResolvesVia [("dfDriverId", "d1"), ("dfFeeType", "RECURRING_EXECUTION_INVOICE")] @?= ["dfDriverId"],
          testCase "feeType In [..]: even the pinned-value combo defers to the regular key" $
            dfResolvesVia [("dfDriverId", "d1"), ("dfFeeType", "CANCELLATION_PENALTY")] @?= ["dfDriverId"],
          testCase "pinned-only rental query: served from the pinned serviceName set" $
            dfResolvesVia [("dfServiceName", "YATRI_RENTAL")] @?= ["dfServiceName"],
          testCase "pinned-only with non-pinned value: no keys, clean DB fallback" $
            dfResolvesVia [("dfServiceName", "YATRI_SUBSCRIPTION")] @?= [],
          testCase "two pinned fields, both values pinned, no regular key: both sets usable" $
            dfResolvesVia [("dfFeeType", "CANCELLATION_PENALTY"), ("dfServiceName", "YATRI_RENTAL")]
              @?= ["dfFeeType", "dfServiceName"],
          testCase "two pinned fields, one value not pinned: only the pinned-value field is used" $
            dfResolvesVia [("dfFeeType", "CANCELLATION_PENALTY"), ("dfServiceName", "YATRI_SUBSCRIPTION")]
              @?= ["dfFeeType"]
        ],
      testGroup
        "DriverFee shape: update / updateAll / delete (same locator + set transitions)"
        [ testCase "update by driverId + serviceName (dues clearing): row located via driverId" $
            dfResolvesVia [("dfDriverId", "d1"), ("dfServiceName", "YATRI_SUBSCRIPTION")] @?= ["dfDriverId"],
          testCase "updateStatusByIds (id In [..]): each combo located via the primary key" $
            dfResolvesVia [("dfId", "fee-1")] @?= ["dfId"],
          testCase "updateAll by driverId In [..] + serviceName: each driver combo located via driverId" $
            (dfResolvesVia [("dfDriverId", "d1"), ("dfServiceName", "YATRI_SUBSCRIPTION")], dfResolvesVia [("dfDriverId", "d2"), ("dfServiceName", "YATRI_SUBSCRIPTION")])
              @?= (["dfDriverId"], ["dfDriverId"]),
          testCase "delete by driverId: located via driverId, pinned fields ignored" $
            dfResolvesVia [("dfDriverId", "d1"), ("dfFeeType", "RECURRING_INVOICE")] @?= ["dfDriverId"],
          testCase "update flips feeType to CANCELLATION_PENALTY: row joins the pinned set, leaves none" $
            ( dfSetsJoined (dfRow "RECURRING_INVOICE" "YATRI_SUBSCRIPTION") (dfRow "CANCELLATION_PENALTY" "YATRI_SUBSCRIPTION"),
              dfSetsLeft (dfRow "RECURRING_INVOICE" "YATRI_SUBSCRIPTION") (dfRow "CANCELLATION_PENALTY" "YATRI_SUBSCRIPTION")
            )
              @?= ([[("dfFeeType", "CANCELLATION_PENALTY")]], []),
          testCase "update flips feeType back: row leaves the pinned set, joins none" $
            ( dfSetsJoined (dfRow "CANCELLATION_PENALTY" "YATRI_SUBSCRIPTION") (dfRow "RECURRING_INVOICE" "YATRI_SUBSCRIPTION"),
              dfSetsLeft (dfRow "CANCELLATION_PENALTY" "YATRI_SUBSCRIPTION") (dfRow "RECURRING_INVOICE" "YATRI_SUBSCRIPTION")
            )
              @?= ([], [[("dfFeeType", "CANCELLATION_PENALTY")]]),
          testCase "update moves service to rental: joins serviceName set, driverId membership untouched" $
            ( dfSetsJoined (dfRow "RECURRING_INVOICE" "YATRI_SUBSCRIPTION") (dfRow "RECURRING_INVOICE" "YATRI_RENTAL"),
              dfSetsLeft (dfRow "RECURRING_INVOICE" "YATRI_SUBSCRIPTION") (dfRow "RECURRING_INVOICE" "YATRI_RENTAL")
            )
              @?= ([[("dfServiceName", "YATRI_RENTAL")]], []),
          testCase "status-only update: no set membership changes at all" $
            ( dfSetsJoined (dfRow "RECURRING_INVOICE" "YATRI_SUBSCRIPTION") ((dfRow "RECURRING_INVOICE" "YATRI_SUBSCRIPTION") {dfStatus = "CLEARED"}),
              dfSetsLeft (dfRow "RECURRING_INVOICE" "YATRI_SUBSCRIPTION") ((dfRow "RECURRING_INVOICE" "YATRI_SUBSCRIPTION") {dfStatus = "CLEARED"})
            )
              @?= ([], [])
        ]
    ]
