module Beckn.Types.SlidingWindowCounters where

import Beckn.Prelude (HasField)
import Beckn.Utils.Dhall (FromDhall)
import Data.Time (UTCTime)
import EulerHS.Prelude

data SlidingWindowOptions = SlidingWindowOptions
  { period :: Integer,
    periodType :: PeriodType
  }
  deriving (Generic, FromDhall, Show)

data PeriodType = Minutes | Hours | Days | Months | Years deriving (Generic, FromDhall, Show, Eq)

type TimePair = (UTCTime, UTCTime) -- (startTime, endTime)

type HasWindowOptions r = HasField "windowOptions" r SlidingWindowOptions
