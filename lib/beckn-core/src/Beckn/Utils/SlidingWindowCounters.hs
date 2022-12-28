module Beckn.Utils.SlidingWindowCounters
  ( incrementWindowCount,
    incrementByValue,
    incrementByValueInTimeBucket,
    getLatestRatio,
    getCurrentWindowValues,
    makeSlidingWindowKey,
    splitOnPeriodGranuality,
    incrementPeriod,
    convertPeriodTypeToSeconds,
    HasWindowOptions,
  )
where

import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.SlidingWindowCounters
import Control.Monad.Extra (fromMaybeM, mapMaybeM)
import qualified Data.Text as T
import Data.Time
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)

-- ========================== Helper functions ==========================

splitOnPeriodGranuality :: PeriodType -> TimePair -> [TimePair]
splitOnPeriodGranuality periodType (startTime, endTime)
  | startTime > endTime = []
  | otherwise = go startTime (incrementPeriod periodType startTime)
  where
    go lastTime newTime
      | newTime < endTime = (lastTime, newTime) : go newTime (incrementPeriod periodType newTime)
      | otherwise = [(lastTime, endTime)]

-- | incrementPeriod :: PeriodType -> timeStamp -> nextRoumdedupTime
--  this function gives the rounded up time for the given timestamp
--  based on the periodType.
--  example : executing >> incrementPeriod Hours "02:14 PM"
--            gives you >> 03:00 PM
--  How it works ?
--   we get the number of seconds as integer that have been passed
--   in the given day. Then based on the period type, we divide that
--   by the unit of seconds in that periodType. i.e. 60 sec for a minute.
--   which gives us a quotient, and incrementing the quotient by 1 and
--   then multuplying by the unit seconds in period type you get the
--   next rounded up timestamp (seconds passed since midnight).
--
--   example -> >> timestamp = 11:40 AM , periodType = Hours
--              >> secondPassedSinceMidNight (sp) = 42000
--              >> unitTimeInPeriodType = 3600 (seconds in an hour)
--              >> 42000 / 3600 = 11
--              >> 3600 * 12 = 43200 (which equals 12:00 PM)
--   ( other implemtations details can be seen in code, like adding days when
--     number of seconds exceed the total seconds a day can have etc.)
incrementPeriod :: PeriodType -> UTCTime -> UTCTime
incrementPeriod periodType (UTCTime date time) = do
  let secs = diffTimeToPicoseconds time `div` (10 ^ (12 :: Integer))
      secInPeriod = convertPeriodTypeToSeconds periodType
      quotient = secs `div` secInPeriod
      newTime' = secInPeriod * (quotient + 1)
      res
        | periodType `elem` [Minutes, Hours, Days] =
          if newTime' >= 86400 -- number of secongs in a day
            then (addDays (newTime' `div` 86400) date, 0)
            else (date, fromIntegral newTime')
        | periodType == Months = do
          let (year, month, _) = toGregorian date
              newMonth' = month + 1
              (newYear, newMonth) = if newMonth' > 12 then (year + 1, 1) else (year, newMonth')
              newDate = fromGregorian newYear newMonth 1
          (newDate, 0)
        | otherwise = do
          let (year, _, _) = toGregorian date
              newDate = fromGregorian (year + 1) 1 1
          (newDate, 0)
  uncurry UTCTime res

makeSlidingWindowKey :: PeriodType -> Text -> UTCTime -> Text
makeSlidingWindowKey pt k = (<> "-sliding-window") . makeTimeBasedKey pt k

makeTimeBasedKey :: PeriodType -> Text -> UTCTime -> Text
makeTimeBasedKey periodType oldKey = do
  (\periodString -> oldKey <> "-" <> periodString)
    . T.pack
    . formatTime
      defaultTimeLocale
      ( case periodType of
          Minutes -> "%Y-%m-%d-%H-%M"
          Hours -> "%Y-%m-%d-%H"
          Days -> "%Y-%m-%d"
          Months -> "%Y-%m"
          Years -> "%Y"
      )

-- gives time in second
convertPeriodTypeToSeconds :: PeriodType -> Integer
convertPeriodTypeToSeconds periodType =
  case periodType of
    Minutes -> 60
    Hours -> 60 * 60
    Days -> 60 * 60 * 24
    Months -> 31 * 60 * 60 * 24
    Years -> 365 * 31 * 60 * 60 * 24

getkeysForLastPeriods :: SlidingWindowOptions -> UTCTime -> (UTCTime -> Text) -> [Text]
getkeysForLastPeriods SlidingWindowOptions {..} utcTime keyModifier =
  map
    ( keyModifier
        . flip addUTCTime utcTime
        . fromInteger
        . getTimeUnit
    )
    [0 .. period - 1]
  where
    getTimeUnit :: Integer -> Integer
    getTimeUnit magnitude = -1 * magnitude * convertPeriodTypeToSeconds periodType

-- ========================== Sliding Window Counters ==========================

incrementWindowCount ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  Text ->
  SlidingWindowOptions ->
  m ()
incrementWindowCount = incrementCounter makeSlidingWindowKey

incrementByValue ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  Integer ->
  Text ->
  SlidingWindowOptions ->
  m ()
incrementByValue val = incrementByValueImpl Nothing val makeSlidingWindowKey

incrementByValueInTimeBucket ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  UTCTime ->
  Integer ->
  Text ->
  SlidingWindowOptions ->
  m ()
incrementByValueInTimeBucket utcTime val = incrementByValueImpl (Just utcTime) val makeSlidingWindowKey

incrementCounter ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  (PeriodType -> Text -> UTCTime -> Text) ->
  Text ->
  SlidingWindowOptions ->
  m ()
incrementCounter = incrementByValueImpl Nothing 1

incrementByValueImpl ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  Maybe UTCTime ->
  Integer ->
  (PeriodType -> Text -> UTCTime -> Text) ->
  Text ->
  SlidingWindowOptions ->
  m ()
incrementByValueImpl mbTimeStamp val keyModifier key SlidingWindowOptions {..} = do
  utcTime <- fromMaybeM (L.runIO getCurrentTime) (pure mbTimeStamp)
  let finalKey = keyModifier periodType key utcTime
  let expirationTime = period * convertPeriodTypeToSeconds periodType
  void $ Redis.incrby finalKey val
  Redis.expire finalKey $ fromIntegral expirationTime

-- ================= Getter functions for fetching window results ======================

-- Logic :
--  get last n (n=period) postivie and total counts from redis, add them up and simply divide them
--  How it works ?
--    SlidingWindowOptions = {period = 3, periodType = Days}
--
--           day 1 counts | day 2 counts | day 3 counts | day 4 counts | now counts
--                                       | -------------------------------------- |
--                                       Σ counts.TIMEBASED_KEY_FOR_POSITIVE_CASE
--                            result =  ___________________________________________
--                                      Σ counts.TIMEBASED_KEY_FOR_THE_TOTAL_CASES
--
--           day 1 counts | day 2 counts | day 3 counts | now counts
--                        | -------------------------------------- |
--                         Σ counts.TIMEBASED_KEY_FOR_POSITIVE_CASE
--              result =  ___________________________________________
--                         Σ counts.TIMEBASED_KEY_FOR_THE_TOTAL_CASES
--
--    ** counts = {<TIMEBASED_KEY_FOR_POSITIVE_CASE>: positiveCases , <TIMEBASED_KEY_FOR_THE_TOTAL_CASES>: totalCases}

-- | getLatestRatio :: (id to getResult for, and generate TIMEBASED_KEY_FOR_THE_TOTAL_CASES) -> (id modifier to create TIMEBASED_KEY_FOR_POSITIVE_CASE) -> Resultsant Ratio of the sliding window
getLatestRatio ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  Text ->
  (Text -> Text) ->
  (Text -> Text) ->
  SlidingWindowOptions ->
  m Double
getLatestRatio driverId mkPostiveCaseKeyfn mkTotalCaseKeyfn s@SlidingWindowOptions {..} = do
  utcTime <- L.runIO getCurrentTime
  let positiveCaseKeysList = getkeysForLastPeriods s utcTime $ makeSlidingWindowKey periodType (mkPostiveCaseKeyfn driverId)
  let totalCountKeysList = getkeysForLastPeriods s utcTime $ makeSlidingWindowKey periodType (mkTotalCaseKeyfn driverId)
  positiveCases <- nonZero . sum <$> mapMaybeM Redis.get positiveCaseKeysList
  totalCases <- nonZero . sum <$> mapMaybeM Redis.get totalCountKeysList
  pure $ positiveCases / totalCases
  where
    nonZero :: Double -> Double
    nonZero a = if a == 0.0 then 1.0 else a

getCurrentWindowValues ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    FromJSON a
  ) =>
  Text ->
  SlidingWindowOptions ->
  m [Maybe a]
getCurrentWindowValues key swo = do
  utcTime <- L.runIO getCurrentTime
  let keysToFetch = getkeysForLastPeriods swo utcTime $ makeSlidingWindowKey (periodType swo) key
  mapM Redis.get keysToFetch
