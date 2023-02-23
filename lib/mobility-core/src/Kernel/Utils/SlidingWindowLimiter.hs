 {-
  Copyright 2022-23, Juspay India Pvt Ltd
  
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
  
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is 
  
  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
  
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero 
  
  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.SlidingWindowLimiter where

import Data.Time hiding (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import EulerHS.Prelude hiding (id)
import GHC.Records.Extra
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common as Common
import Kernel.Types.Error
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.Common hiding (nominalDiffTimeToSeconds)

checkSlidingWindowLimit ::
  ( Redis.HedisFlow m r,
    HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions]
  ) =>
  Text ->
  m ()
checkSlidingWindowLimit key = do
  limitOptions <- asks (.apiRateLimitOptions)
  checkSlidingWindowLimitWithOptions key limitOptions

checkSlidingWindowLimitWithOptions ::
  ( Redis.HedisFlow m r,
    MonadTime m
  ) =>
  Text ->
  APIRateLimitOptions ->
  m ()
checkSlidingWindowLimitWithOptions key APIRateLimitOptions {..} = do
  unlessM (slidingWindowLimiter key limit limitResetTimeInSec) $
    throwError $ HitsLimitError limitResetTimeInSec

-- Sliding window rate limiter.
-- Returns True if limit is not exceed and further
-- actions should be allowed. False otherwise.

slidingWindowLimiter :: (Redis.HedisFlow m r, MonadTime m) => Text -> Int -> Int -> m Bool
slidingWindowLimiter key frameHitsLim frameLen = do
  currTime <- getCurrentTime
  hits <- fromMaybe [] <$> Redis.get key
  let (filtHits, ret) = slidingWindowLimiterPure currTime hits frameHitsLim frameLen
  when ret $ Redis.setExp key filtHits frameLen
  return ret

slidingWindowLimiterPure :: UTCTime -> [Integer] -> Int -> Int -> ([Integer], Bool)
slidingWindowLimiterPure currTime hits frameHitsLim frameLen = do
  -- How it works:
  -- We convert UTCTime value to Integer and `div` it by frameLen to
  -- get its frame number. After that we getting list with
  -- timeFrames from redis and getting number of calls within
  -- current and previous frame. Getting prevFrameWeight
  -- (timePassedSinceCurrFrameStart/frameLen == 1 >= n >= 0) and
  -- doing check (prevFrameHitsLen * prevFrameWeight + currFrameHitsLen < frameHitsLim).
  -- If passed - add currFrame to frames list, save it in redis and return True. False otherwise.
  let currFrame = getTimeFrame currTime
      filtHits = filter (hitsFilter currFrame) hits
      prevFrameHitsLen = length $ filter (prevFrameHitsFilter currFrame) filtHits
      prevFrameWeight = 1 - (fromIntegral (getTimeWithinFrame currTime) :: Double) / frameLen'
      currFrameHitsLen = length $ filter (currFrameHitsFilter currFrame) filtHits
      res = floor (fromIntegral prevFrameHitsLen * prevFrameWeight) + currFrameHitsLen < frameHitsLim
  (if res then currFrame : filtHits else filtHits, res)
  where
    frameLen' :: Num a => a
    frameLen' = fromIntegral frameLen
    getTimeFrame time = getTime time `div` frameLen'
    getTimeWithinFrame time = getTime time `mod` frameLen'
    getTime :: UTCTime -> Integer
    getTime = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
    hitsFilter currFrame timeFrame = (timeFrame == currFrame - 1) || (timeFrame == currFrame)
    prevFrameHitsFilter currFrame timeFrame = timeFrame == currFrame - 1
    currFrameHitsFilter currFrame timeFrame = timeFrame == currFrame
