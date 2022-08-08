{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Types.Common
  ( module Beckn.Types.Common,
    module Common,
    HasField,
  )
where

import Beckn.External.Encryption as Common (EncFlow)
import Beckn.External.FCM.Types as Common (FCMFlow)
import Beckn.Prelude
import Beckn.Storage.DB.Config as Common (DBFlow)
import Beckn.Storage.Esqueleto.Config as Common (EsqDBFlow)
import Beckn.Types.App as Common
import Beckn.Types.Forkable as Common
import Beckn.Types.GuidLike as Common
import Beckn.Types.Logging as Common
import Beckn.Types.MonadGuid as Common
import Beckn.Types.Time as Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.GenericPretty
import Data.Generics.Labels ()
import Database.Persist.Class
import Database.Persist.Sql
import GHC.Float (double2Int, int2Double)
import GHC.Records.Extra (HasField)

newtype IdObject = IdObject
  { id :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype Meters = Meters
  { getMeters :: Int
  }
  deriving newtype (Show, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum, ToSchema, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

newtype HighPrecMeters = HighPrecMeters
  { getHighPrecMeters :: Double
  }
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Fractional, Real, Ord, Eq, Enum, ToSchema, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

newtype Kilometers = Kilometers
  { getKilometers :: Int
  }
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum, ToSchema, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

kilometersToMeters :: Kilometers -> Meters
kilometersToMeters (Kilometers n) = Meters $ n * 1000

metersToKilometers :: Meters -> Kilometers
metersToKilometers (Meters n) = Kilometers $ n `div` 1000

metersToHighPrecMeters :: Meters -> HighPrecMeters
metersToHighPrecMeters (Meters n) = HighPrecMeters $ int2Double n

highPrecMetersToMeters :: HighPrecMeters -> Meters
highPrecMetersToMeters (HighPrecMeters n) = Meters $ double2Int n

--
newtype Money = Money Int
  deriving newtype (Show, PrettyShow, Enum, Eq, Ord, Num, Real, Integral, PersistField, PersistFieldSql, ToJSON, FromJSON, ToSchema)
