{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.SlidingWindowCounters where

import Beckn.Storage.Esqueleto (derivePersistFieldJSON)
import Beckn.Utils.Dhall (FromDhall)
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
