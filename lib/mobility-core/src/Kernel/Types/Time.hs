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

module Kernel.Types.Time where

import Data.Aeson (Value (..))
import Data.Aeson.Types (typeMismatch)
import Data.OpenApi (ToSchema)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Database.Persist.Class
import Database.Persist.Sql
import EulerHS.Prelude
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.GenericPretty
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
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum, ToSchema, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

newtype Hours = Hours
  { getHours :: Int
  }
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum, ToSchema, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

newtype Days = Days
  { getDays :: Int
  }
  deriving newtype (Show, Read, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum, ToSchema, PrettyShow, PersistField, PersistFieldSql)
  deriving stock (Generic)

daysToSeconds :: Days -> Seconds
daysToSeconds = Seconds . (* 86400) . getDays

type MeasuringDuration m a = MonadClock m => m a -> m a

class Monad m => MonadTime m where
  getCurrentTime :: m UTCTime

class Monad m => MonadClock m where
  getClockTime :: m Clock.TimeSpec

instance MonadTime IO where
  getCurrentTime = Time.getCurrentTime

instance MonadClock IO where
  getClockTime = Clock.getTime Clock.Monotonic

newtype Iso8601Time = Iso8601Time {getUtcTime :: UTCTime}
  deriving (Show, Eq)
  deriving newtype (ToSchema, PrettyShow)

instance FromJSON Iso8601Time where
  parseJSON (String s) = Iso8601Time <$> iso8601ParseM (Text.unpack s)
  parseJSON e = typeMismatch "Iso8601Time String" e

instance ToJSON Iso8601Time where
  toJSON (Iso8601Time t) = String . Text.pack $ iso8601Show t
