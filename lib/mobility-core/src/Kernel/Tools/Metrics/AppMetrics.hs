module Kernel.Tools.Metrics.AppMetrics where

import Data.Time.Clock
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Hedis
import Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Tools.Metrics.CoreMetrics
import qualified System.Environment as SE

startToSelectRedisKey :: Text -> Text
startToSelectRedisKey requestId = "start_to_select_redis_key_" <> requestId

shouldPushLatencyMetrics :: IO Bool
shouldPushLatencyMetrics = fromMaybe False . (>>= readMaybe) <$> SE.lookupEnv "SHOULD_PUSH_LATENCY_METRICS"

redisKeyExpiryTime :: Int
redisKeyExpiryTime = 300

startSelectToSendRequestLatency ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    HedisFlow m r,
    MonadReader r m
  ) =>
  Text ->
  m ()
startSelectToSendRequestLatency requestId = do
  shouldPush <- liftIO shouldPushLatencyMetrics
  when shouldPush $ do
    handle (\(e :: SomeException) -> L.logError ("START_METRICS_PUSH_FAILED" :: Text) $ "Error in startSelectToSendRequestLatency: " <> show e) $ do
      startTime <- liftIO getCurrentTime
      Hedis.setExp (startToSelectRedisKey requestId) startTime redisKeyExpiryTime

finishSelectToSendRequestLatency ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    HedisFlow m r,
    MonadReader r m
  ) =>
  Text ->
  m ()
finishSelectToSendRequestLatency requestId = do
  shouldPush <- liftIO shouldPushLatencyMetrics
  when shouldPush $ do
    handle (\(e :: SomeException) -> L.logError ("FINISH_METRICS_PUSH_FAILED" :: Text) $ "Error in finishSelectToSendRequestLatency: " <> show e) $ do
      endTime <- liftIO getCurrentTime
      startTime <- Hedis.withCrossAppRedis $ Hedis.safeGet (startToSelectRedisKey requestId)
      case startTime of
        Just (a :: UTCTime) -> do
          let latency = diffUTCTime endTime a & nominalDiffTimeToSeconds & (* 1000) & round
          addSelectToSendRequestLatency requestId latency
        Nothing -> pure ()
