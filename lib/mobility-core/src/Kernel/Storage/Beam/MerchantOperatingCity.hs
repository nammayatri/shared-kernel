{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Kernel.Storage.Beam.MerchantOperatingCity where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Prelude

data MerchantOperatingCityT f = MerchantOperatingCityT
  { id :: B.C f Kernel.Prelude.Text,
    city :: B.C f Kernel.Prelude.Text,
    stdCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantOperatingCityT where
  data PrimaryKey MerchantOperatingCityT f = MerchantOperatingCityId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantOperatingCityId . id

type MerchantOperatingCity = MerchantOperatingCityT Identity

$(enableKVPG ''MerchantOperatingCityT ['id] [])

$(mkTableInstancesGenericSchema ''MerchantOperatingCityT "merchant_operating_city")
