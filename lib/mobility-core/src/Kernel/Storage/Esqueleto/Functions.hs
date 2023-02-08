{-# OPTIONS_GHC -Wno-deprecations #-}

module Kernel.Storage.Esqueleto.Functions
  ( (<->.),
    getPoint,
    containsPoint,
    IntervalVal (..),
    interval,
    rand,
    unnest,
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

containsPoint :: (Double, Double) -> SqlExpr (Value b)
containsPoint (lon, lat) = unsafeSqlFunction "st_contains" args
  where
    args = (unsafeSqlValue "geom", geomFromText pointText)
    geomFromText = unsafeSqlFunction "ST_GeomFromText"
    pointText = val ("POINT (" <> show lon <> " " <> show lat <> ")") :: SqlExpr (Value Text)

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
