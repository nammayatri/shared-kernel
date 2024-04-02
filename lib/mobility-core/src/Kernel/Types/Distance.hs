{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn=identities #-}

module Kernel.Types.Distance where

import Data.Aeson
import Data.OpenApi hiding (value)
import Database.Beam
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Persist.Class
import Database.Persist.Sql
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import GHC.Float (double2Int, int2Double)
import GHC.Records.Extra (HasField)
import Kernel.Prelude as KP
import Kernel.Types.Centesimal
import Kernel.Types.FromField
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.GenericPretty
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Sequelize.SQLObject (SQLObject (..), ToSQLObject (convertToSQLObject))
import Servant
import Text.Show (Show (..))

newtype HighPrecDistance = HighPrecDistance
  { getHighPrecDistance :: Rational
  }
  deriving stock (Generic)
  deriving newtype (Num, FromDhall, Real, Fractional, RealFrac, Ord, Eq, Enum, PrettyShow, PersistField, PersistFieldSql)

instance Show HighPrecDistance where
  show = Text.Show.show @Double . realToFrac

instance Read HighPrecDistance where
  readsPrec d s = do
    (dobuleVal, s1) :: (Double, String) <- readsPrec d s
    return (realToFrac dobuleVal, s1)

instance ToJSON HighPrecDistance where
  toJSON = toJSON @Double . realToFrac

instance FromJSON HighPrecDistance where
  parseJSON = fmap realToFrac . parseJSON @Double

instance FromField HighPrecDistance where
  fromField f mbValue = HighPrecDistance <$> fromFieldDefault f mbValue

instance HasSqlValueSyntax be Rational => HasSqlValueSyntax be HighPrecDistance where
  sqlValueSyntax = sqlValueSyntax . getHighPrecDistance

instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecDistance

instance FromBackendRow Postgres HighPrecDistance

instance ToSchema HighPrecDistance where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Double)
    return $ NamedSchema (Just "HighPrecDistance") aSchema

$(mkHttpInstancesForEnum ''HighPrecDistance)

data DistanceUnit = Meter | Mile | Yard | Kilometer
  deriving stock (Generic, Show, Read, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
  deriving (PrettyShow) via Showable DistanceUnit

-- cycle imports

-- $(mkBeamInstancesForEnum ''DistanceUnit)

instance FromField DistanceUnit where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DistanceUnit where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DistanceUnit

instance FromBackendRow Postgres DistanceUnit

-- convertToMeters :: Distance -> Distance
-- convertToMeters d@(Distance _ _ Meter) = d
-- convertToMeters (Distance _ v unit) = do
--   let v' =
--         v * case unit of
--           Mile -> 1609.34
--           Kilometer -> 1000
--           Yard -> 0.9144
--   Distance (roundToIntegral v') v' Meter

convertToMeters :: Distance -> Distance
convertToMeters d@(Distance _ Meter) = d
convertToMeters (Distance v unit) = do
  let v' =
        v * case unit of
          Mile -> 1609.34
          Kilometer -> 1000
          Yard -> 0.9144
  Distance v' Meter

data Distance = Distance
  { -- valueInt :: Int, -- To be deprecated
    value :: HighPrecDistance,
    unit :: DistanceUnit
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
  deriving (PrettyShow) via Showable Distance

instance Eq Distance where
  a == b = withUnitChecking a b (\_unit -> (==))

instance Ord Distance where
  a <= b = withUnitChecking a b (\_unit -> (<=))

-- do we need this instance?
instance Num Distance where
  a + b = withUnitChecking a b (\unit a' b' -> Distance (a' + b') unit)
  a - b = withUnitChecking a b (\unit a' b' -> Distance (a' - b') unit)
  a * b = withUnitChecking a b (\unit a' b' -> Distance (a' * b') unit) -- should not be used
  negate = modifyDistanceValue negate
  abs = modifyDistanceValue abs
  signum = modifyDistanceValue signum
  fromInteger a = Distance (fromInteger a) Meter -- should not be used

modifyDistanceValue :: (HighPrecDistance -> HighPrecDistance) -> Distance -> Distance
modifyDistanceValue func d = d{value = func d.value}

withUnitChecking ::
  Distance ->
  Distance ->
  (DistanceUnit -> HighPrecDistance -> HighPrecDistance -> a) ->
  a
withUnitChecking d1 d2 func =
  if (d1.unit == d2.unit)
    then func d1.unit d1.value d2.value
    else func Meter ((convertToMeters d1).value) ((convertToMeters d2).value)

-- data DistanceAPIEntity = DistanceAPIEntity
--   { value :: HighPrecDistance,
--     unit :: DistanceUnit
--   }
--   deriving stock (Generic, Show)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

-- mkDistanceAPIEntity :: Distance -> DistanceAPIEntity
-- mkDistanceAPIEntity Distance {..} = DistanceAPIEntity {..}

newtype Meters = Meters
  { getMeters :: Int
  }
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

newtype HighPrecMeters = HighPrecMeters
  { getHighPrecMeters :: Centesimal
  }
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Fractional, Real, RealFrac, Ord, Eq, Enum, ToSchema, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

newtype Kilometers = Kilometers
  { getKilometers :: Int
  }
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum, ToSchema, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

deriving newtype instance FromField Kilometers

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Kilometers where
  sqlValueSyntax = sqlValueSyntax . getKilometers

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Kilometers

instance FromBackendRow Postgres Kilometers

kilometersToMeters :: Kilometers -> Meters
kilometersToMeters (Kilometers n) = Meters $ n * 1000

metersToKilometers :: Meters -> Kilometers
metersToKilometers (Meters n) = Kilometers $ n `div` 1000

metersToHighPrecMeters :: Meters -> HighPrecMeters
metersToHighPrecMeters (Meters n) = HighPrecMeters . realToFrac $ int2Double n

highPrecMetersToMeters :: HighPrecMeters -> Meters
highPrecMetersToMeters (HighPrecMeters n) = Meters . double2Int $ realToFrac n

instance HasSqlValueSyntax be Centesimal => HasSqlValueSyntax be HighPrecMeters where
  sqlValueSyntax = sqlValueSyntax . getHighPrecMeters

instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecMeters

instance FromBackendRow Postgres HighPrecMeters

instance FromField HighPrecMeters where
  fromField f mbValue = HighPrecMeters <$> fromFieldDefault f mbValue

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Meters where
  sqlValueSyntax = sqlValueSyntax . getMeters

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

instance FromBackendRow Postgres Meters

instance FromField Meters where
  fromField = fromFieldJSON

instance {-# OVERLAPPING #-} ToSQLObject Meters where
  convertToSQLObject = SQLObjectValue . KP.show . getMeters

mkDistanceWithDefaultMeters :: Maybe DistanceUnit -> Maybe HighPrecDistance -> Meters -> Distance
mkDistanceWithDefaultMeters mbUnit mbValue defDistance = case mbValue of
  Just value ->
    Distance
      { value,
        unit = fromMaybe Meter mbUnit
      }
  Nothing -> metersToDistance defDistance

mkDistanceWithDefault :: Maybe DistanceUnit -> Maybe HighPrecDistance -> HighPrecMeters -> Distance
mkDistanceWithDefault mbUnit mbValue defDistance = case mbValue of
  Just value ->
    Distance
      { value,
        unit = fromMaybe Meter mbUnit
      }
  Nothing -> highPrecMetersToDistance defDistance

distanceToMeters :: Distance -> Meters
distanceToMeters = Meters . round @HighPrecDistance @Int . (.value) . convertToMeters

distanceToHighPrecMeters :: Distance -> HighPrecMeters
distanceToHighPrecMeters = realToFrac @HighPrecDistance @HighPrecMeters . (.value) . convertToMeters

metersToDistance :: Meters -> Distance
metersToDistance meters =
  Distance
    { value = realToFrac @Meters @HighPrecDistance meters,
      unit = Meter
    }

highPrecMetersToDistance :: HighPrecMeters -> Distance
highPrecMetersToDistance highPrecMeters =
  Distance
    { value = realToFrac @HighPrecMeters @HighPrecDistance highPrecMeters,
      unit = Meter
    }
