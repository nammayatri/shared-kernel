{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Kernel.Types.Time where

import Data.OpenApi (ToParamSchema, ToSchema)
import Data.Time (UTCTime)
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres (Postgres)
import Database.Persist.Class
import Database.Persist.Sql
import Database.PostgreSQL.Simple.FromField (FromField)
import EulerHS.Prelude
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.GenericPretty
import Servant (FromHttpApiData, ToHttpApiData)
import qualified System.Clock as Clock

newtype Microseconds = Microseconds
  { getMicroseconds :: Int
  }
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum, ToSchema, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

newtype Milliseconds = Milliseconds
  { getMilliseconds :: Int
  }
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum, ToSchema, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

newtype Seconds = Seconds
  { getSeconds :: Int
  }
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum, ToSchema, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

newtype Minutes = Minutes
  { getMinutes :: Int
  }
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

newtype Hours = Hours
  { getHours :: Int
  }
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum, ToSchema, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

deriving newtype instance FromField Hours

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Hours where
  sqlValueSyntax = sqlValueSyntax . getHours

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Hours

instance FromBackendRow Postgres Hours

newtype Days = Days
  { getDays :: Int
  }
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum, ToSchema, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

daysToSeconds :: Days -> Seconds
daysToSeconds = Seconds . (* 86400) . getDays

minutesToSeconds :: Minutes -> Seconds
minutesToSeconds = Seconds . (* 60) . getMinutes

newtype Months = Months
  { getMonths :: Int
  }
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum, ToSchema, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

deriving newtype instance FromField Months

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Months where
  sqlValueSyntax = sqlValueSyntax . getMonths

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Months

instance FromBackendRow Postgres Months

type MeasuringDuration m a = MonadClock m => m a -> m a

class Monad m => MonadTime m where
  getCurrentTime :: m UTCTime

class Monad m => MonadClock m where
  getClockTime :: m Clock.TimeSpec

instance MonadTime IO where
  getCurrentTime = Time.getCurrentTime

instance MonadClock IO where
  getClockTime = Clock.getTime Clock.Monotonic

data TimeWithZone = TimeWithZone
  { twzTime :: UTCTime,
    twzZone :: TZone
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TZone = UTC | IST
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- Beam instances for TimeWithZone
-- Store ONLY UTCTime in database (no schema change!)
instance HasSqlValueSyntax be UTCTime => HasSqlValueSyntax be TimeWithZone where
  sqlValueSyntax (TimeWithZone utcTime _) = sqlValueSyntax utcTime

-- ✅ Only stores UTCTime, ignores TZone

instance FromBackendRow Postgres TimeWithZone where
  fromBackendRow = do
    utcTime <- fromBackendRow @Postgres @UTCTime
    pure $ TimeWithZone utcTime IST

-- ✅ Always reconstructs with IST timezone

instance BeamSqlBackend be => B.HasSqlEqualityCheck be TimeWithZone

instance BeamSqlBackend be => B.HasSqlQuantifiedEqualityCheck be TimeWithZone
