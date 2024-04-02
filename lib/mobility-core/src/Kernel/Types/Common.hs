{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wwarn=identities #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Kernel.Types.Common
  ( module Kernel.Types.Common,
    module Common,
    HasField,
  )
where

import Data.Aeson
import Data.ByteString.Internal (ByteString)
import Data.Fixed (Centi, Fixed (MkFixed))
import Data.Generics.Labels ()
import Data.OpenApi hiding (value)
import Data.Text as T
import qualified Data.Vector as V
import Database.Beam
import qualified Database.Beam as B
import Database.Beam.Backend
import qualified Database.Beam.Backend.SQL.AST as B
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import qualified Database.Beam.Query as BQ
import Database.Persist.Class
import Database.Persist.Sql
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import GHC.Float (double2Int, int2Double)
import GHC.Records.Extra (HasField)
import Kernel.External.Encryption
import Kernel.External.Encryption as Common (EncFlow)
import Kernel.Prelude as KP
import Kernel.Storage.Esqueleto.Config as Common (EsqDBFlow)
import Kernel.Storage.Esqueleto.Types
import Kernel.Types.App as Common
import Kernel.Types.Centesimal as Common
import Kernel.Types.Forkable as Common
import Kernel.Types.FromField as Common
import Kernel.Types.GuidLike as Common
import Kernel.Types.Logging as Common
import Kernel.Types.MonadGuid as Common
import Kernel.Types.Price as Common
import Kernel.Types.Time as Common
import Kernel.Utils.Dhall (FromDhall, Natural)
import Kernel.Utils.GenericPretty
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Sequelize.SQLObject (SQLObject (..), ToSQLObject (convertToSQLObject))
import Servant
import Text.Show (Show (..))

newtype IdObject = IdObject
  { id :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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

convertToMeters :: Distance -> Distance
convertToMeters d@(Distance _ _ Meter) = d
convertToMeters (Distance _ v unit) = do
  let v' =
        v * case unit of
          Mile -> 1609.34
          Kilometer -> 1000
          Yard -> 0.9144
  Distance (roundToIntegral v') v' Meter

data Distance = Distance
  { valueInt :: Int, -- To be deprecated
    value :: HighPrecDistance,
    unit :: DistanceUnit
  }
  deriving stock (Generic, Show)
  deriving (PrettyShow) via Showable Distance

data DistanceAPIEntity = DistanceAPIEntity
  { value :: HighPrecDistance,
    unit :: DistanceUnit
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

mkDistanceAPIEntity :: Distance -> DistanceAPIEntity
mkDistanceAPIEntity Distance {..} = DistanceAPIEntity {..}

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

data KVTable = KVTable
  { nameOfTable :: Text,
    percentEnable :: Natural,
    redisTtl :: Maybe Integer
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, FromDhall, Ord)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be KVTable where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be KVTable

instance FromBackendRow Postgres KVTable

instance FromField KVTable where
  fromField = fromFieldJSON

data Tables = Tables
  { enableKVForWriteAlso :: [KVTable],
    enableKVForRead :: [Text],
    useCAC :: [Text],
    useCACForFrontend :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, FromDhall)

data KafkaProperties = KafkaProperties
  { propName :: Text,
    propValue :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, FromDhall)

data KvConfigLastUpdatedTime = KvConfigLastUpdatedTime UTCTime
  deriving (Generic, Show, ToJSON, FromJSON, FromDhall)

data KvConfigUpdateFrequency = KvConfigUpdateFrequency Int
  deriving (Generic, Show, ToJSON, FromJSON, FromDhall)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Tables where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Tables

instance FromBackendRow Postgres Tables

instance FromField Tables where
  fromField = fromFieldJSON

instance FromField Centi where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Minutes where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Minutes

instance FromBackendRow Postgres Minutes

-- FIXME isn't it the same as deriving newtype?
instance FromField Minutes where
  fromField f mbValue = Minutes <$> fromFieldDefault f mbValue

newtype CentiDouble = CentiDouble Double

-- Conversion functions
centiToDouble :: Centi -> CentiDouble
centiToDouble (MkFixed n) = CentiDouble (fromIntegral n / 100)

doubleToCenti :: CentiDouble -> Centi
doubleToCenti (CentiDouble d) = MkFixed (round (d * 100))

-- Define HasSqlValueSyntax instances
instance HasSqlValueSyntax be Double => HasSqlValueSyntax be CentiDouble where
  sqlValueSyntax (CentiDouble d) = sqlValueSyntax d

instance HasSqlValueSyntax be Double => HasSqlValueSyntax be Centi where
  sqlValueSyntax = sqlValueSyntax . centiToDouble

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

instance FromBackendRow Postgres Centesimal

instance FromField Centesimal where
  fromField = fromFieldEnum

instance HasSqlValueSyntax B.Value (V.Vector Text) where
  sqlValueSyntax = autoSqlValueSyntax

instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [Text] where
  sqlValueSyntax x = sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Text]

instance FromBackendRow Postgres [Text]

instance FromField [Text] where
  fromField f mbValue = V.toList <$> fromField f mbValue

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centi

instance FromBackendRow Postgres Centi

instance HasSqlValueSyntax be Centi => HasSqlValueSyntax be Centesimal where
  sqlValueSyntax = sqlValueSyntax . getCenti

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

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Seconds where
  sqlValueSyntax = sqlValueSyntax . getSeconds

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

instance FromBackendRow Postgres Seconds

instance FromField Seconds where
  fromField f mbValue = Seconds <$> fromFieldDefault f mbValue

instance FromField ByteString => FromField DbHash where
  fromField f mb = do
    val <- fromField f mb
    pure $ DbHash val

instance HasSqlValueSyntax be ByteString => HasSqlValueSyntax be DbHash where
  sqlValueSyntax = sqlValueSyntax . unDbHash

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DbHash

instance FromBackendRow Postgres DbHash

instance (HasSqlValueSyntax be (V.Vector Int)) => HasSqlValueSyntax be [Int] where
  sqlValueSyntax x = sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Int]

instance FromBackendRow Postgres [Int]

instance FromField [Int] where
  fromField f mbValue = V.toList <$> fromField f mbValue

getPoint :: (Double, Double) -> BQ.QGenExpr context Postgres s Point
getPoint (lat, lon) = BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "ST_SetSRID (ST_Point (" <> KP.show lon <> " , " <> KP.show lat <> "),4326)"))

containsPoint'' :: (Double, Double) -> BQ.QGenExpr context Postgres s BQ.SqlBool
containsPoint'' (lon, lat) = B.sqlBool_ (BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "st_contains (" <> KP.show lon <> " , " <> KP.show lat <> ")")))

containsPoint' :: (Double, Double) -> BQ.QGenExpr context Postgres s BQ.SqlBool
containsPoint' (lon, lat) = B.sqlBool_ (BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "st_contains (geom, ST_GeomFromText('POINT (" <> KP.show lon <> " " <> KP.show lat <> ")'))")))

buildRadiusWithin' :: Point -> (Double, Double) -> Int -> BQ.QGenExpr context Postgres s BQ.SqlBool
buildRadiusWithin' pnt (lat, lon) rad =
  BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "ST_DWithin(" <> KP.show pnt <> " , " <> getPoint' <> " , " <> KP.show rad <> ")"))
  where
    getPoint' = "(SRID=4326;POINT(" <> KP.show lon <> " " <> KP.show lat <> "))"

buildRadiusWithin'' :: (Double, Double) -> Int -> BQ.QGenExpr context Postgres s BQ.SqlBool
buildRadiusWithin'' (lat, lon) rad =
  BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "ST_DWithin(point" <> " , " <> getPoint' <> " , " <> KP.show rad <> ")"))
  where
    getPoint' = "(ST_SetSRID (ST_Point (" <> KP.show lon <> " , " <> KP.show lat <> "),4326))"

(<->.) :: Point -> Point -> BQ.QGenExpr context Postgres s Double
(<->.) p1 p2 = BQ.QExpr (\_ -> PgExpressionSyntax (emit $ KP.show p1 <> " <-> " <> KP.show p2))
