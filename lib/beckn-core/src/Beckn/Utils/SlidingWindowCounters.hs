module Beckn.Utils.SlidingWindowCounters
  ( incrementWindowCount,
    incrementTotalCount,
    incrementByValue,
    getLatestRatio,
    getCurrentWindowValues,
  )
where

import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.SlidingWindowCounters
import Control.Monad.Extra (mapMaybeM)
import qualified Data.Text as T
import Data.Time
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)

-- ========================== Helper functions ==========================
makeSlidingWindowKey :: PeriodType -> Text -> LocalTime -> Text
makeSlidingWindowKey pt k = (<> "-sliding-window") . makeTimeBasedKey pt k

makeTotalCountKey :: PeriodType -> Text -> LocalTime -> Text
makeTotalCountKey pt k = (<> "-total-count") . makeSlidingWindowKey pt k

makeTimeBasedKey :: PeriodType -> Text -> LocalTime -> Text
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
    Months -> 30 * 60 * 60 * 24
    Years -> 365 * 30 * 60 * 60 * 24

getLocalTime :: IO LocalTime
getLocalTime = do
  currTime <- getCurrentTime
  timeZone <- getTimeZone currTime
  pure $ utcToLocalTime timeZone currTime

getkeysForLastPeriods :: SlidingWindowOptions -> LocalTime -> (LocalTime -> Text) -> [Text]
getkeysForLastPeriods SlidingWindowOptions {..} localTime keyModifier =
  map
    ( keyModifier
        . flip addLocalTime localTime
        . fromInteger
        . getTimeUnit
    )
    [0 .. period -1]
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

incrementTotalCount ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  Text ->
  SlidingWindowOptions ->
  m ()
incrementTotalCount = incrementCounter makeTotalCountKey

incrementByValue ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  Integer ->
  Text ->
  SlidingWindowOptions ->
  m ()
incrementByValue val = incrementByValueImpl val makeSlidingWindowKey

incrementCounter ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  (PeriodType -> Text -> LocalTime -> Text) ->
  Text ->
  SlidingWindowOptions ->
  m ()
incrementCounter = incrementByValueImpl 1

incrementByValueImpl ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  Integer ->
  (PeriodType -> Text -> LocalTime -> Text) ->
  Text ->
  SlidingWindowOptions ->
  m ()
incrementByValueImpl val keyModifier key SlidingWindowOptions {..} = do
  localTime <- L.runIO getLocalTime
  let finalKey = keyModifier periodType key localTime
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
  SlidingWindowOptions ->
  m Double
getLatestRatio driverId mkPostiveCaseKeyfn s@SlidingWindowOptions {..} = do
  localTime <- L.runIO getLocalTime
  let positiveCaseKeysList = getkeysForLastPeriods s localTime $ makeSlidingWindowKey periodType (mkPostiveCaseKeyfn driverId)
  let totalCountKeysList = getkeysForLastPeriods s localTime $ makeTotalCountKey periodType driverId
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
  localTime <- L.runIO getLocalTime
  let keysToFetch = getkeysForLastPeriods swo localTime $ makeSlidingWindowKey (periodType swo) key
  mapM Redis.get keysToFetch
