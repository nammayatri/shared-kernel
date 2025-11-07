module Kernel.Tools.Metrics.AppMetrics where

import Data.String.Conversions
import Data.Time.Clock
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Hedis
import Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.TryException
import qualified System.Environment as SE

data LatencyAction = SELECT_TO_SEND_REQUEST
  deriving (Show, Eq, Generic)

genericLatencyRedisKey :: Text -> Text
genericLatencyRedisKey txnId = "glk_" <> txnId

shouldPushLatencyMetrics :: IO Bool
shouldPushLatencyMetrics = fromMaybe False . (>>= readMaybe) <$> SE.lookupEnv "SHOULD_PUSH_LATENCY_METRICS"

redisKeyExpiryTime :: Int
redisKeyExpiryTime = 300

startGenericLatencyMetrics ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    HedisFlow m r,
    MonadReader r m,
    TryException m
  ) =>
  LatencyAction ->
  Text ->
  m ()
startGenericLatencyMetrics action txnId = do
  shouldPush <- liftIO shouldPushLatencyMetrics
  when shouldPush $ do
    handle (\(e :: SomeException) -> L.logError ("START_METRICS_PUSH_FAILED" :: Text) $ "Error in startGenericRequestLatency: for action: " <> show action <> " txnId: " <> txnId <> " error: " <> show e) $ do
      startTime <- liftIO getCurrentTime
      withCrossAppRedis $ Hedis.setExp (cs $ genericLatencyRedisKey txnId) startTime redisKeyExpiryTime

finishGenericLatencyMetrics ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    HedisFlow m r,
    MonadReader r m,
    TryException m
  ) =>
  LatencyAction ->
  Text ->
  m ()
finishGenericLatencyMetrics action txnId = do
  shouldPush <- liftIO shouldPushLatencyMetrics
  when shouldPush $ do
    handle (\(e :: SomeException) -> L.logError ("FINISH_METRICS_PUSH_FAILED" :: Text) $ "Error in finishGenericRequestLatency: for action: " <> show action <> " txnId: " <> txnId <> " error: " <> show e) $ do
      endTime <- liftIO getCurrentTime
      startTime <- Hedis.withCrossAppRedis $ Hedis.safeGet (genericLatencyRedisKey txnId)
      case startTime of
        Just (a :: UTCTime) -> do
          -- get diff in Seconds
          let latency = diffUTCTime endTime a & nominalDiffTimeToSeconds & round
          addGenericLatencyMetrics (show action) latency
        Nothing -> do
          pure ()
