{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Kernel.Storage.Esqueleto.Functions
  ( (<->.),
    getPoint,
    containsPoint,
    getGeomGeoJSON,
    IntervalVal (..),
    interval,
    rand,
    unnest,
    buildRadiusWithin,
    buildRegionWithin,
    containsRegion,
    getTextFromGeoJSON,
    geojsonToBin,
    containsPointGeom,
  )
where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Database.Esqueleto.Experimental as Esq
import Database.Esqueleto.Internal.Internal hiding (rand)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Types

(<->.) :: SqlExpr (Value Point) -> SqlExpr (Value Point) -> SqlExpr (Value Double)
(<->.) = unsafeSqlBinOp " <-> "

getPoint :: (SqlExpr (Value Double), SqlExpr (Value Double)) -> SqlExpr (Value Point)
getPoint (lat, long) = unsafeSqlFunction "ST_SetSRID" (buildSTPoint (long, lat), val (4326 :: Int))

buildSTPoint :: (SqlExpr (Value Double), SqlExpr (Value Double)) -> SqlExpr (Value b)
buildSTPoint = unsafeSqlFunction "ST_Point"

getGeomGeoJSON :: SqlExpr (Value Text)
getGeomGeoJSON = unsafeSqlFunction "ST_AsGeoJSON" args
  where
    args = unsafeSqlValue "geom"

getTextFromGeoJSON :: SqlExpr (Value Text) -> SqlExpr (Value Text)
getTextFromGeoJSON geoJson = unsafeSqlFunction "ST_SetSRID" args
  where
    args = (unsafeSqlFunction "ST_GeomFromGeoJSON" geoJson, val (4326 :: Int))

geojsonToBin :: SqlExpr (Value Text) -> SqlExpr (Value Geom)
geojsonToBin = unsafeSqlFunction "geojson_to_bin"

buildRadiusWithin :: SqlExpr (Value Point) -> (Double, Double) -> SqlExpr (Value Int) -> SqlExpr (Value b)
buildRadiusWithin pnt (lat, lon) radius = unsafeSqlFunction "ST_DWithin" args
  where
    args = (pnt, getPoint', radius)
    getPoint' = val ("SRID=4326;POINT(" <> show lon <> " " <> show lat <> ")") :: SqlExpr (Value Text)

containsPoint :: (Double, Double) -> SqlExpr (Value b)
containsPoint (lon, lat) = unsafeSqlFunction "st_contains" args
  where
    args = (unsafeSqlValue "geom", geomFromText pointText)
    geomFromText = unsafeSqlFunction "ST_GeomFromText"
    pointText = val ("POINT (" <> show lon <> " " <> show lat <> ")") :: SqlExpr (Value Text)

containsPointGeom :: (Double, Double) -> SqlExpr (Value b)
containsPointGeom (lon, lat) = unsafeSqlFunction "st_contains" args
  where
    args = (unsafeSqlValue "geom", geomFromText gftArgs)
    geomFromText = unsafeSqlFunction "ST_GeomFromText"
    gftArgs = (pointText, val (4326 :: Int))
    pointText = val ("POINT (" <> show lon <> " " <> show lat <> ")") :: SqlExpr (Value Text)

containsRegion :: (Double, Double) -> (Double, Double) -> SqlExpr (Value b)
containsRegion (minLon, minLat) (maxLon, maxLat) = unsafeSqlFunction "st_intersects" args
  where
    args = (unsafeSqlValue "geom", unsafeSqlFunction "ST_MakeEnvelope" points)
    points = (val minLon, val minLat, val maxLon, val maxLat, val (4326 :: Int))

buildRegionWithin :: (Double, Double) -> (Double, Double) -> SqlExpr (Value b)
buildRegionWithin (minLat, minLon) (maxLat, maxLon) = unsafeSqlFunction "ST_MakeEnvelope" args
  where
    args = (val minLon, val minLat, val maxLon, val maxLat)

data IntervalVal = YEAR Int | MONTH Int | DAY Int | HOUR Int | MINUTE Int | SECOND Int deriving (Show)

interval :: [IntervalVal] -> SqlExpr (Value UTCTime)
interval intervalVals = unsafeSqlValue valueString
  where
    valueString = "interval '" <> intervalArg <> "'"
    intervalArg = TL.fromLazyText $ TL.unwords (intervalValToString <$> intervalVals)
    intervalValToString = \case
      YEAR i -> show i <> " YEAR"
      MONTH i -> show i <> " MONTH"
      DAY i -> show i <> " DAY"
      HOUR i -> show i <> " HOUR"
      MINUTE i -> show i <> " MINUTE"
      SECOND i -> show i <> " SECOND"

rand :: SqlExpr OrderBy
rand = Esq.rand

unnest :: PostgresListField a => SqlExpr (Value a) -> SqlExpr (Value b)
unnest = unsafeSqlFunction "unnest"
