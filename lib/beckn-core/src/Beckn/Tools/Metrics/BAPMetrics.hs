module Beckn.Tools.Metrics.BAPMetrics
  ( module Beckn.Tools.Metrics.BAPMetrics,
    module Reexport,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Tools.Metrics.BAPMetrics.Types as Reexport
import Beckn.Types.Common
import Data.Time (diffUTCTime)
import GHC.Records.Extra
import Prometheus as P

startSearchMetrics :: (Redis.HedisFlow m r, HasBAPMetrics m r) => Text -> m ()
startSearchMetrics txnId = do
  bmContainer <- asks (.bapMetrics)
  startSearchMetrics' bmContainer txnId

finishSearchMetrics :: (Redis.HedisFlow m r, HasBAPMetrics m r) => Text -> m ()
finishSearchMetrics txnId = do
  bmContainer <- asks (.bapMetrics)
  finishSearchMetrics' bmContainer txnId

incrementSearchRequestCount :: HasBAPMetrics m r => m ()
incrementSearchRequestCount = do
  bmContainer <- asks (.bapMetrics)
  incrementCaseCount' bmContainer

incrementCaseCount' :: MonadIO m => BAPMetricsContainer -> m ()
incrementCaseCount' bmContainer = do
  let searchRequestCounter = bmContainer.searchRequestCounter
  liftIO $ P.incCounter searchRequestCounter --is it correct that Euler.runIO = liftIO?

putSearchDuration :: MonadIO m => P.Histogram -> Double -> m ()
putSearchDuration searchDurationHistogram duration = liftIO $ P.observe searchDurationHistogram duration

searchDurationKey :: Text -> Text
searchDurationKey txnId = "beckn:" <> txnId <> ":on_search:received"

searchDurationLockKey :: Text -> Text
searchDurationLockKey txnId = txnId <> ":on_search"

startSearchMetrics' :: (Redis.HedisFlow m r, MonadFlow m) => BAPMetricsContainer -> Text -> m ()
startSearchMetrics' bmContainer txnId = do
  let (_, failureCounter) = bmContainer.searchDuration
      searchRedisExTime = getSeconds bmContainer.searchDurationTimeout
  startTime <- getCurrentTime
  Redis.setExp (searchDurationKey txnId) startTime (searchRedisExTime + 1) -- a bit more time to
  -- allow forked thread to handle failure
  fork "Gateway Search Metrics" $ do
    liftIO $ threadDelay $ searchRedisExTime * 1000000
    whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
      Redis.get (searchDurationKey txnId) >>= \case
        Just (_ :: UTCTime) -> do
          void $ Redis.del (searchDurationKey txnId)
          liftIO $ P.incCounter failureCounter
        Nothing -> return ()
      Redis.unlockRedis $ searchDurationLockKey txnId

finishSearchMetrics' :: (Redis.HedisFlow m r, MonadTime m) => BAPMetricsContainer -> Text -> m ()
finishSearchMetrics' bmContainer txnId = do
  let (searchDurationHistogram, _) = bmContainer.searchDuration
      searchRedisExTime = getSeconds bmContainer.searchDurationTimeout
  endTime <- getCurrentTime
  whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
    Redis.get (searchDurationKey txnId) >>= \case
      Just startTime -> do
        void $ Redis.del (searchDurationKey txnId)
        putSearchDuration searchDurationHistogram . realToFrac . diffUTCTime endTime $ startTime
      Nothing -> return ()
    Redis.unlockRedis $ searchDurationLockKey txnId
