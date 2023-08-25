{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Types.SlidingWindowCounters where

import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistFieldJSON)
import Kernel.Utils.Dhall (FromDhall)

data SlidingWindowOptions = SlidingWindowOptions
  { period :: Integer,
    periodType :: PeriodType
  }
  deriving (Read, Generic, FromDhall, Show, FromJSON, ToJSON, ToSchema, Ord, Eq)

fromFieldSWC ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion SlidingWindowOptions
fromFieldSWC f mbValue = do
  value <- fromField f mbValue
  case A.fromJSON value of
    A.Success a -> pure a
    _ -> DPSF.returnError DPSF.ConversionFailed f "Conversion failed"

instance FromField SlidingWindowOptions where
  fromField = fromFieldSWC

instance HasSqlValueSyntax be A.Value => HasSqlValueSyntax be SlidingWindowOptions where
  sqlValueSyntax = sqlValueSyntax . A.toJSON

instance BeamSqlBackend be => B.HasSqlEqualityCheck be SlidingWindowOptions

instance FromBackendRow Postgres SlidingWindowOptions

instance IsString SlidingWindowOptions where
  fromString = show

data PeriodType = Minutes | Hours | Days | Months | Years deriving (Read, Generic, FromDhall, Show, Eq, FromJSON, ToJSON, ToSchema, Ord)

type TimePair = (UTCTime, UTCTime) -- (startTime, endTime)

derivePersistFieldJSON "SlidingWindowOptions"
