 {-
  Copyright 2022-23, Juspay India Pvt Ltd
  
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
  
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is 
  
  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
  
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero 
  
  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Time
  ( module Kernel.Utils.Time,
    module Kernel.Types.Time,
    UTCTime,
    addUTCTime,
    diffUTCTime,
  )
where

import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, nominalDiffTimeToSeconds)
import qualified Data.Time as Time
import EulerHS.Prelude
import Kernel.Types.Time
import Kernel.Utils.Logging
import System.Clock (toNanoSecs)

isExpired :: MonadTime m => NominalDiffTime -> UTCTime -> m Bool
isExpired nominal time = do
  now <- getCurrentTime
  let addedUTCTime = addUTCTime nominal time
  return $ now > addedUTCTime

-- | Format time in IST and return it as text
-- Converts and Formats in the format
-- TODO: make a generic function and then pass format
-- and timezone as arguments. Currently adds +5:30
showTimeIst :: UTCTime -> Text
showTimeIst time =
  T.pack $
    formatTime defaultTimeLocale "%d %b, %I:%M %p" $
      addUTCTime (60 * 330) time

getClockTimeInMs :: MonadClock m => m Milliseconds
getClockTimeInMs = Milliseconds . fromInteger . (`div` 1000000) . toNanoSecs <$> getClockTime

measureDuration :: MonadClock m => m a -> m (a, Milliseconds)
measureDuration f = do
  start <- getClockTimeInMs
  res <- f
  end <- getClockTimeInMs
  return (res, end - start)

measuringDuration :: (Milliseconds -> a -> m ()) -> MeasuringDuration m a
measuringDuration doWithDuration f = do
  (res, dur) <- measureDuration f
  doWithDuration dur res
  return res

measuringDurationToLog :: Log m => LogLevel -> Text -> MeasuringDuration m a
measuringDurationToLog logLevel fname = tabs . measuringDuration $ \duration _ ->
  withLogTag "duration"
    . logOutput logLevel
    $ fname <> " took " <> show duration <> " milliseconds"
  where
    -- debugging feature, use only in dev
    -- tabs = (withLogTag "  " .)
    tabs = id

millisecondsToMicroseconds :: Milliseconds -> Microseconds
millisecondsToMicroseconds (Milliseconds mill) = Microseconds $ mill * 1000

secondsToMinutes :: Seconds -> Minutes
secondsToMinutes (Seconds secs) = Minutes $ secs `div` 60

secondsToMcs :: Seconds -> Microseconds
secondsToMcs (Seconds s) = Microseconds (s * 1000000)

secondsToMillis :: Seconds -> Milliseconds
secondsToMillis (Seconds s) = Milliseconds (s * 1000)

secondsToNominalDiffTime :: Seconds -> NominalDiffTime
secondsToNominalDiffTime = millisToNominalDiffTime . secondsToMillis

nominalDiffTimeToSeconds :: NominalDiffTime -> Seconds
nominalDiffTimeToSeconds = round . Time.nominalDiffTimeToSeconds

millisToSecondsDouble :: Milliseconds -> Double
millisToSecondsDouble (Milliseconds ms) = fromIntegral ms / 1000

millisToNominalDiffTime :: Milliseconds -> NominalDiffTime
millisToNominalDiffTime = realToFrac @Double @NominalDiffTime . millisToSecondsDouble

threadDelayMilliSec :: (MonadIO m) => Milliseconds -> m ()
threadDelayMilliSec milli = liftIO $ threadDelay $ milli.getMilliseconds * 1000

threadDelaySec :: (MonadIO m) => Seconds -> m ()
threadDelaySec sec = liftIO $ threadDelay $ sec.getSeconds * 1000000

compareTimeWithInterval :: NominalDiffTime -> UTCTime -> UTCTime -> Ordering
compareTimeWithInterval dt time1 time2
  | abs (diffUTCTime time1 time2) < abs dt = EQ
  | time1 < time2 = LT
  | otherwise = GT -- time1 > time2
