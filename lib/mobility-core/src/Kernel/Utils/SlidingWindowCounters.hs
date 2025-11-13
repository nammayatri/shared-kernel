{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.SlidingWindowCounters
  ( incrementWindowCount,
    incrementByValue,
    incrementByValueInTimeBucket,
    decrementWindowCount,
    getkeysForLastPeriods,
    makeSWKeyForTime,
    getCurrentWindowValuesUptoLast,
    getLatestRatio,
    getCurrentWindowCount,
    getCurrentWindowValues,
    makeSlidingWindowKey,
    splitOnPeriodGranuality,
    incrementPeriod,
    convertPeriodTypeToSeconds,
    deleteCurrentWindowValues,
  )
where

import qualified Control.Monad.Extra as CME
import qualified Data.Text as T
import Data.Time
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.SlidingWindowCounters
import Kernel.Types.TryException

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

makeCachingLockKey :: Text -> Text
makeCachingLockKey key = ("SW-CACHE-FOR-" <> key)

makeSlidingWindowKey :: PeriodType -> Text -> UTCTime -> Text
makeSlidingWindowKey pt k = (<> "-sliding-window") . makeTimeBasedKey pt k

makeQuickAccessWindowCountKey :: Text -> Text
makeQuickAccessWindowCountKey = (<> "-sliding-window-result")

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
getkeysForLastPeriods swo utcTime keyModifier = map (makeSWKeyForTime swo utcTime keyModifier) [0 .. swo.period - 1]

getkeysUptoThisPeriod :: SlidingWindowOptions -> UTCTime -> (UTCTime -> Text) -> Integer -> [Text]
getkeysUptoThisPeriod swo utcTime keyModifier uptoPeriod = map (makeSWKeyForTime swo utcTime keyModifier) [0 .. uptoPeriod - 1]

makeSWKeyForTime :: SlidingWindowOptions -> UTCTime -> (UTCTime -> Text) -> Integer -> Text
makeSWKeyForTime SlidingWindowOptions {..} utcTime keyModifier periodMagnitude =
  keyModifier
    . flip addUTCTime utcTime
    . fromInteger
    $ getTimeUnit
  where
    getTimeUnit :: Integer
    getTimeUnit = -1 * periodMagnitude * convertPeriodTypeToSeconds periodType

-- ========================== Sliding Window Counters ==========================

incrementWindowCount ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    TryException m
  ) =>
  Text ->
  SlidingWindowOptions ->
  m ()
incrementWindowCount = incrementCounter makeSWKeyForTime makeQuickAccessWindowCountKey makeSlidingWindowKey

incrementByValue ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    TryException m
  ) =>
  Integer ->
  Text ->
  SlidingWindowOptions ->
  m ()
incrementByValue val = incrementByValueImpl Nothing val makeSWKeyForTime makeQuickAccessWindowCountKey makeSlidingWindowKey

incrementByValueInTimeBucket ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    TryException m
  ) =>
  UTCTime ->
  Integer ->
  Text ->
  SlidingWindowOptions ->
  m ()
incrementByValueInTimeBucket utcTime val = incrementByValueImpl (Just utcTime) val makeSWKeyForTime makeQuickAccessWindowCountKey makeSlidingWindowKey

incrementCounter ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    TryException m
  ) =>
  (SlidingWindowOptions -> UTCTime -> (UTCTime -> Text) -> Integer -> Text) ->
  (Text -> Text) ->
  (PeriodType -> Text -> UTCTime -> Text) ->
  Text ->
  SlidingWindowOptions ->
  m ()
incrementCounter = incrementByValueImpl Nothing 1

incrementByValueImpl ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    TryException m
  ) =>
  Maybe UTCTime ->
  Integer ->
  (SlidingWindowOptions -> UTCTime -> (UTCTime -> Text) -> Integer -> Text) ->
  (Text -> Text) ->
  (PeriodType -> Text -> UTCTime -> Text) ->
  Text ->
  SlidingWindowOptions ->
  m ()
incrementByValueImpl mbTimeStamp val getOutOfWindowKey getStoredResultKey getWindowKey key swo@SlidingWindowOptions {..} = do
  now <- L.runIO getCurrentTime
  let utcTime = fromMaybe now mbTimeStamp
      finalKey = getWindowKey periodType key utcTime
  Redis.whenWithLockRedis (makeCachingLockKey key) 10 . void $ cacheTheCounts now val getOutOfWindowKey getStoredResultKey getWindowKey key swo
  let expirationTime = (period + 1) * convertPeriodTypeToSeconds periodType
  void $ Redis.incrby finalKey val
  Redis.expire finalKey $ fromIntegral expirationTime

decrementWindowCount ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    TryException m
  ) =>
  Text ->
  SlidingWindowOptions ->
  m ()
decrementWindowCount = decrementCounter makeSWKeyForTime makeQuickAccessWindowCountKey makeSlidingWindowKey

decrementCounter ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    TryException m
  ) =>
  (SlidingWindowOptions -> UTCTime -> (UTCTime -> Text) -> Integer -> Text) ->
  (Text -> Text) ->
  (PeriodType -> Text -> UTCTime -> Text) ->
  Text ->
  SlidingWindowOptions ->
  m ()
decrementCounter = decrementByValueImpl Nothing 1

decrementByValueImpl ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    TryException m
  ) =>
  Maybe UTCTime ->
  Integer ->
  (SlidingWindowOptions -> UTCTime -> (UTCTime -> Text) -> Integer -> Text) ->
  (Text -> Text) ->
  (PeriodType -> Text -> UTCTime -> Text) ->
  Text ->
  SlidingWindowOptions ->
  m ()
decrementByValueImpl mbTimeStamp val getOutOfWindowKey getStoredResultKey getWindowKey key swo@SlidingWindowOptions {..} = do
  now <- L.runIO getCurrentTime
  let utcTime = fromMaybe now mbTimeStamp
      finalKey = getWindowKey periodType key utcTime
  Redis.whenWithLockRedis (makeCachingLockKey key) 10 . void $ cacheTheCounts now val getOutOfWindowKey getStoredResultKey getWindowKey key swo
  let expirationTime = (period + 1) * convertPeriodTypeToSeconds periodType
  void $ Redis.decrby finalKey val
  Redis.expire finalKey $ fromIntegral expirationTime

-- the cached value would stay correct for current and current + 1 peroid in any given periodType so keeping the expiry as end of (current + 1) periodType from now
cacheTheCounts ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    TryException m
  ) =>
  UTCTime ->
  Integer ->
  (SlidingWindowOptions -> UTCTime -> (UTCTime -> Text) -> Integer -> Text) ->
  (Text -> Text) ->
  (PeriodType -> Text -> UTCTime -> Text) ->
  Text ->
  SlidingWindowOptions ->
  m Integer
cacheTheCounts now val getOutOfWindowKey getStoredResultKey getWindowKey key swo@SlidingWindowOptions {..} = do
  let storedResultKey = getStoredResultKey key
  mbOldStoredResult <- Redis.get storedResultKey
  (updatedValueToStore, shouldRecache) <-
    case mbOldStoredResult of
      Just oldStoredResult -> do
        let noLongerPartOfWindowKey = getOutOfWindowKey swo now (getWindowKey periodType key) period
        mbValToRemoveFromWindow <- Redis.get noLongerPartOfWindowKey
        let finalValToRemoveFromWindow =
              case mbValToRemoveFromWindow of
                Just valToRemoveFromWindow | valToRemoveFromWindow > 0 -> valToRemoveFromWindow
                _ -> 0
            newStoreValue = max 0 $ oldStoredResult + val - finalValToRemoveFromWindow
        whenJust mbValToRemoveFromWindow $ \_ -> Redis.del noLongerPartOfWindowKey
        if newStoreValue == oldStoredResult
          then pure (newStoreValue, False)
          else pure (newStoreValue, True)
      Nothing -> (,True) . (+ val) . sum . catMaybes <$> getCurrentWindowValues key swo
  bool
    (pure updatedValueToStore)
    ( do
        let storedResultExpTime = fromInteger $ floor (diffUTCTime (incrementPeriod periodType now) now) + (convertPeriodTypeToSeconds periodType)
        void $ Redis.setExp storedResultKey updatedValueToStore storedResultExpTime
        pure updatedValueToStore
    )
    shouldRecache

-- ================= Getter functions for fetching window results during first calculation ======================

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
-- Minutes | Hours | Days | Months | Years
getCountsFromCache :: (L.MonadFlow m, Redis.HedisFlow m r, TryException m) => Text -> m (Maybe Integer)
getCountsFromCache key = do
  let storesResultKey = makeQuickAccessWindowCountKey key
  Redis.get storesResultKey

cacheAndGetNumDeno ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    TryException m
  ) =>
  UTCTime ->
  Text ->
  (Text -> Text) ->
  (Text -> Text) ->
  SlidingWindowOptions ->
  m (Integer, Integer)
cacheAndGetNumDeno now driverId mkPostiveCaseKeyfn mkTotalCaseKeyfn swo = do
  let totalCaseKey = mkTotalCaseKeyfn driverId
      positiveCaseKey = mkPostiveCaseKeyfn driverId
  totalCases <- cacheTheCounts now 0 makeSWKeyForTime makeQuickAccessWindowCountKey makeSlidingWindowKey totalCaseKey swo
  positiveCases <- cacheTheCounts now 0 makeSWKeyForTime makeQuickAccessWindowCountKey makeSlidingWindowKey positiveCaseKey swo
  pure (positiveCases, totalCases)

getLatestRatio ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    TryException m
  ) =>
  Text ->
  (Text -> Text) ->
  (Text -> Text) ->
  SlidingWindowOptions ->
  m Double
getLatestRatio driverId mkPostiveCaseKeyfn mkTotalCaseKeyfn slidingWindowOptions = do
  now <- L.runIO getCurrentTime
  (positiveCases, totalCases) <-
    CME.fromMaybeM
      (cacheAndGetNumDeno now driverId mkPostiveCaseKeyfn mkTotalCaseKeyfn slidingWindowOptions)
      (getNumDenFromCache (mkPostiveCaseKeyfn driverId) (mkTotalCaseKeyfn driverId))
  return $ fromIntegral positiveCases / fromIntegral (max 1 totalCases)

getNumDenFromCache :: (L.MonadFlow m, Redis.HedisFlow m r, TryException m) => Text -> Text -> m (Maybe (Integer, Integer))
getNumDenFromCache postiveCaseKey totalCaseKey =
  runMaybeT $ do
    positiveCases <- MaybeT $ getCountsFromCache postiveCaseKey
    totalCases <- MaybeT $ getCountsFromCache totalCaseKey
    pure (positiveCases, totalCases)

getCurrentWindowCount ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    TryException m
  ) =>
  Text ->
  SlidingWindowOptions ->
  m Integer
getCurrentWindowCount key swo = do
  now <- L.runIO getCurrentTime
  mbCountFromCache <- getCountsFromCache key
  case mbCountFromCache of
    Just count -> pure count
    Nothing -> Redis.withWaitAndLockRedis (makeCachingLockKey key) 10 20 $ cacheTheCounts now 0 makeSWKeyForTime makeQuickAccessWindowCountKey makeSlidingWindowKey key swo

getCurrentWindowValuesUptoLast ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    FromJSON a,
    TryException m
  ) =>
  Integer ->
  Text ->
  SlidingWindowOptions ->
  m [Maybe a]
getCurrentWindowValuesUptoLast nPeriod key swo = do
  utcTime <- L.runIO getCurrentTime
  let keysToFetch = getkeysUptoThisPeriod swo utcTime (makeSlidingWindowKey (periodType swo) key) nPeriod
  mapM Redis.get keysToFetch

getCurrentWindowValues ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    FromJSON a,
    TryException m
  ) =>
  Text ->
  SlidingWindowOptions ->
  m [Maybe a]
getCurrentWindowValues key swo = do
  utcTime <- L.runIO getCurrentTime
  let keysToFetch = getkeysForLastPeriods swo utcTime $ makeSlidingWindowKey (periodType swo) key
  mapM Redis.get keysToFetch

deleteCurrentWindowValues ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    TryException m
  ) =>
  Text ->
  SlidingWindowOptions ->
  m ()
deleteCurrentWindowValues key swo = do
  utcTime <- L.runIO getCurrentTime
  let keysToDel = getkeysForLastPeriods swo utcTime $ makeSlidingWindowKey (periodType swo) key
  mapM_ Redis.del keysToDel
