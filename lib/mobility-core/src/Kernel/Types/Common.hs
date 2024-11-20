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
import qualified Data.HashMap.Strict as HM
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
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import GHC.Records.Extra (HasField)
import Kernel.External.Encryption
import Kernel.External.Encryption as Common (EncFlow)
import Kernel.Prelude as KP
import Kernel.Storage.Esqueleto.Config as Common (EsqDBFlow)
import Kernel.Storage.Esqueleto.Types
import Kernel.Types.App as Common
import Kernel.Types.Centesimal as Common
import Kernel.Types.Distance as Common
import Kernel.Types.Forkable as Common
import Kernel.Types.FromField as Common
import Kernel.Types.GuidLike as Common
import Kernel.Types.Logging as Common
import Kernel.Types.MonadGuid as Common
import Kernel.Types.Price as Common
import Kernel.Types.Time as Common
import Kernel.Utils.Dhall (FromDhall)

newtype IdObject = IdObject
  { id :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Tables = Tables
  { disableForKV :: [Text],
    kvTablesTtl :: HM.HashMap Text Integer,
    useCAC :: [Text],
    useCACForFrontend :: Bool,
    readFromMasterDb :: [Text],
    tableShardModValue :: Maybe (HM.HashMap Text Int)
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
