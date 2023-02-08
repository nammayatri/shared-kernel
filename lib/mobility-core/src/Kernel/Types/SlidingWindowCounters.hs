{-# LANGUAGE TemplateHaskell #-}

module Kernel.Types.SlidingWindowCounters where

import Kernel.Storage.Esqueleto (derivePersistFieldJSON)
import Kernel.Utils.Dhall (FromDhall)
import Data.Time (UTCTime)
import EulerHS.Prelude

data SlidingWindowOptions = SlidingWindowOptions
  { period :: Integer,
    periodType :: PeriodType
  }
  deriving (Read, Generic, FromDhall, Show, FromJSON, ToJSON)

data PeriodType = Minutes | Hours | Days | Months | Years deriving (Read, Generic, FromDhall, Show, Eq, FromJSON, ToJSON)

type TimePair = (UTCTime, UTCTime) -- (startTime, endTime)

derivePersistFieldJSON "SlidingWindowOptions"
