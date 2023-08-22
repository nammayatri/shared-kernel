{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Types.Common
  ( module Kernel.Types.Common,
    module Common,
    HasField,
  )
where

import Data.Aeson
import qualified Data.Bifunctor as BF
import Data.ByteString.Internal (ByteString, unpackChars)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Fixed (Centi, Fixed (MkFixed))
import Data.Generics.Labels ()
import Data.OpenApi
import Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Data.Vector as V
import Database.Beam
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import qualified Database.Beam.Query as BQ
import Database.Persist.Class
import Database.Persist.Sql
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
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
import Kernel.Types.Time as Common
import Kernel.Utils.Dhall (FromDhall, Natural)
import Kernel.Utils.GenericPretty
import Servant
import Text.Show (Show (..))

newtype IdObject = IdObject
  { id :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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

kilometersToMeters :: Kilometers -> Meters
kilometersToMeters (Kilometers n) = Meters $ n * 1000

metersToKilometers :: Meters -> Kilometers
metersToKilometers (Meters n) = Kilometers $ n `div` 1000

metersToHighPrecMeters :: Meters -> HighPrecMeters
metersToHighPrecMeters (Meters n) = HighPrecMeters . realToFrac $ int2Double n

highPrecMetersToMeters :: HighPrecMeters -> Meters
highPrecMetersToMeters (HighPrecMeters n) = Meters . double2Int $ realToFrac n

newtype Money = Money
  { getMoney :: Int
  }
  deriving stock (Generic)
  deriving newtype (Show, Read, PrettyShow, Enum, Eq, Ord, Num, Real, Integral, PersistField, PersistFieldSql, ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData)

newtype HighPrecMoney = HighPrecMoney
  { getHighPrecMoney :: Rational
  }
  deriving stock (Generic)
  deriving newtype (Num, FromDhall, Real, Fractional, RealFrac, Ord, Eq, Enum, PrettyShow, PersistField, PersistFieldSql)

data KVTable = KVTable
  { nameOfTable :: Text,
    percentEnable :: Natural
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, FromDhall)

data Tables = Tables
  { enableKVForWriteAlso :: [KVTable],
    enableKVForRead :: [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, FromDhall)

instance Show HighPrecMoney where
  show = Text.Show.show @Double . realToFrac

instance Read HighPrecMoney where
  readsPrec d s = do
    (dobuleVal, s1) :: (Double, String) <- readsPrec d s
    return (realToFrac dobuleVal, s1)

instance ToJSON HighPrecMoney where
  toJSON = toJSON @Double . realToFrac

instance FromJSON HighPrecMoney where
  parseJSON = fmap realToFrac . parseJSON @Double

instance ToSchema HighPrecMoney where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Double)
    return $ NamedSchema (Just "HighPrecMoney") aSchema

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Money where
  sqlValueSyntax = sqlValueSyntax . getMoney

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

instance FromBackendRow Postgres Money

instance FromField Money where
  fromField = fromFieldJSON

instance IsString Money where
  fromString = KP.show

instance FromField Centi where
  fromField = fromFieldCenti

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Minutes where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Minutes

instance FromBackendRow Postgres Minutes

fromFieldMoney ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Money
fromFieldMoney f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> Money <$> fromField f mbValue

fromFieldCenti ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Centi
fromFieldCenti f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' ->
    case readMaybe (unpackChars value') of
      Just val -> pure val
      _ -> DPSF.returnError ConversionFailed f ("Could not 'read'" <> KP.show value')

fromFieldCentesimal ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Centesimal
fromFieldCentesimal f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> Centesimal <$> fromField f mbValue

-- FIXME next functions are almost the same
fromFieldMinutes ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Minutes
fromFieldMinutes f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> Minutes <$> fromField f mbValue

fromFieldMeters ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Meters
fromFieldMeters f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> Meters <$> fromField f mbValue

fromFieldHighPrecMeters ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion HighPrecMeters
fromFieldHighPrecMeters f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> HighPrecMeters <$> fromField f mbValue

fromFieldHighPrecMoney ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion HighPrecMoney
fromFieldHighPrecMoney f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> HighPrecMoney <$> fromField f mbValue

fromFieldSeconds ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Seconds
fromFieldSeconds f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> Seconds <$> fromField f mbValue

instance FromField Minutes where
  fromField = fromFieldMinutes

instance HasSqlValueSyntax be Integer => HasSqlValueSyntax be Centi where
  sqlValueSyntax (MkFixed i) = sqlValueSyntax (div i 100)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

instance FromBackendRow Postgres Centesimal

instance FromField Centesimal where
  fromField = fromFieldEnum

instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [Text] where
  sqlValueSyntax x = sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Text]

instance FromBackendRow Postgres [Text]

instance FromField [Text] where
  fromField f mbValue = V.toList <$> fromField f mbValue

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centi

instance FromBackendRow Postgres Centi

instance IsString Centi where
  fromString = KP.show

instance HasSqlValueSyntax be Centi => HasSqlValueSyntax be Centesimal where
  sqlValueSyntax = sqlValueSyntax . getCenti

instance HasSqlValueSyntax be Centesimal => HasSqlValueSyntax be HighPrecMeters where
  sqlValueSyntax = sqlValueSyntax . getHighPrecMeters

instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecMeters

instance FromBackendRow Postgres HighPrecMeters

instance FromField HighPrecMeters where
  fromField = fromFieldHighPrecMeters

instance IsString HighPrecMeters where
  fromString = KP.show

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Meters where
  sqlValueSyntax = sqlValueSyntax . getMeters

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

instance FromBackendRow Postgres Meters

instance FromField Meters where
  fromField = fromFieldJSON

instance IsString Meters where
  fromString = KP.show

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Seconds where
  sqlValueSyntax = sqlValueSyntax . getSeconds

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

instance FromBackendRow Postgres Seconds

instance IsString Seconds where
  fromString = KP.show

instance FromField HighPrecMoney where
  fromField = fromFieldHighPrecMoney

instance HasSqlValueSyntax be Rational => HasSqlValueSyntax be HighPrecMoney where
  sqlValueSyntax = sqlValueSyntax . getHighPrecMoney

instance HasSqlValueSyntax be Double => HasSqlValueSyntax be Rational where
  sqlValueSyntax = sqlValueSyntax . (fromRational :: Rational -> Double)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecMoney

instance FromBackendRow Postgres HighPrecMoney

instance IsString HighPrecMoney where
  fromString = KP.show

instance FromField Seconds where
  fromField = fromFieldSeconds

instance FromField ByteString => FromField DbHash where
  fromField f mb = do
    val <- fromField f mb
    pure $ DbHash val

instance HasSqlValueSyntax be ByteString => HasSqlValueSyntax be DbHash where
  sqlValueSyntax = sqlValueSyntax . unDbHash

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DbHash

instance FromBackendRow Postgres DbHash

instance IsString DbHash where
  fromString = KP.show

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

(<->.) :: Point -> Point -> BQ.QGenExpr context Postgres s Double
(<->.) p1 p2 = BQ.QExpr (\_ -> PgExpressionSyntax (emit $ KP.show p1 <> " <-> " <> KP.show p2))

instance FromHttpApiData HighPrecMoney where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . fromStrict

instance ToHttpApiData HighPrecMoney where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = toStrict . encode
