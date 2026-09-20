{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HedisClusterPipeline (hedisClusterPipelineTests) where

import qualified Control.Exception as Exc
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isRight)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Vector as V
import Data.Word (Word16)
import qualified Database.Redis as Hedis
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisCfg (..), HedisEnv (..), connectHedisCluster, defaultHedisCfg, disconnectHedis, mGetClusterRaw, mGetClusterWithKeys, runPipelinedByKey, zAddIfPossibleMany)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common (Log (..), MonadClock (..), MonadTime (..))
import Kernel.Types.TryException (TryException (..))
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Prelude as P

data TestEnv = TestEnv
  { hedisEnv :: HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    secondaryHedisClusterEnv :: Maybe HedisEnv,
    hedisMigrationStage :: Bool,
    enablePrometheusMetricLogging :: Bool,
    enableRedisLatencyLogging :: Bool
  }

newtype TestM a = TestM (ReaderT TestEnv IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader TestEnv, MonadThrow, MonadCatch)

runTestM :: TestEnv -> TestM a -> IO a
runTestM env (TestM action) = runReaderT action env

instance MonadTime TestM where
  getCurrentTime = liftIO getCurrentTime

instance MonadClock TestM where
  getClockTime = liftIO getClockTime

instance Log TestM where
  logOutput _ _ = pure ()
  withLogTag _ action = action

instance TryException TestM where
  withTryCatch _ = try

instance Metrics.CoreMetrics TestM where
  addRequestLatency _ _ _ _ = pure ()
  addDatastoreLatency _ _ _ = pure ()
  incrementErrorCounter _ _ = pure ()
  addUrlCallRetries _ _ = pure ()
  addUrlCallRetryFailures _ = pure ()
  incrementSortedSetCounter _ = pure ()
  incrementStreamCounter _ = pure ()
  incrementStreamFailedCounter _ = pure ()
  addGenericLatency _ _ = pure ()
  incrementSchedulerFailureCounter _ = pure ()
  incrementSchedulerJobDisabledCounter _ = pure ()
  incrementSchedulerJobLifecycleCounter _ _ = pure ()
  addSchedulerProducerStageCount _ _ = pure ()
  incrementGenericMetrics _ = pure ()
  incrementConfigPilotSuccessCounter _ = pure ()
  incrementConfigPilotFailureCounter _ = pure ()
  incrementSystemConfigsFailedCounter _ = pure ()
  incrementRideStartCounter _ = pure ()
  incrementRideEndCounter _ = pure ()
  incrementRedisStreamProcessed = pure ()
  incrementRedisStreamDead _ = pure ()
  setRedisStreamLength _ _ = pure ()
  setRedisStreamPending _ _ = pure ()
  addGenericLatencyMetrics _ _ = pure ()
  addOpenTripPlannerResponse _ _ _ = pure ()
  addOpenTripPlannerLatency _ _ _ = pure ()
  incrementTryExceptionCounter _ _ = pure ()
  withForkCounters _ _ action = action
  incrementProducerError _ = pure ()
  incrementSmsProviderResponseCounter _ _ = pure ()

type Ctx = (TestEnv, Text)

hedisClusterPipelineTests :: IO TestTree
hedisClusterPipelineTests = do
  mbPort <- lookupEnv "MOBILITY_CORE_TEST_REDIS_CLUSTER_PORT"
  pure $ case mbPort >>= readMaybe of
    Nothing -> testGroup "Redis cluster pipelining (skipped: set MOBILITY_CORE_TEST_REDIS_CLUSTER_PORT to a cluster node port)" []
    Just port -> withResource (acquire port) (disconnectHedis . (.hedisClusterEnv) . fst) (testGroup "Redis cluster pipelining" . tests)

acquire :: Word16 -> IO Ctx
acquire port = do
  hedis <- connectHedisCluster defaultHedisCfg {connectPort = port} identity
  now <- getCurrentTime
  let runId = T.pack (P.show (floor (utcTimeToPOSIXSeconds now * 1000) :: Integer))
  pure (TestEnv hedis hedis hedis hedis Nothing False False False, runId)

tests :: IO Ctx -> [TestTree]
tests getCtx =
  [ testCase "mGetClusterRaw returns each value at its key's index across slots and nodes" $ do
      (env, runId) <- getCtx
      let ks = [mkKey runId ("order:" <> tshow i) | i <- [1 .. 600 :: Int]]
          vs = [value i | i <- [1 .. 600 :: Int]]
      assertBool "keys should spread over many hash slots" (distinctSlots ks > 500)
      res <- withKeys env (zip ks vs) $ runTestM env (mGetClusterRaw ks)
      res @?= V.fromList (Just <$> vs),
    testCase "mGetClusterRaw puts Nothing at the index of every missing key" $ do
      (env, runId) <- getCtx
      let ks = [mkKey runId ((if even i then "present:" else "missing:") <> tshow i) | i <- [1 .. 200 :: Int]]
          present = [(k, value i) | (i, k) <- zip [1 :: Int ..] ks, even i]
          expected = [if even i then Just (value i) else Nothing | i <- [1 .. 200 :: Int]]
      res <- withKeys env present $ runTestM env (mGetClusterRaw ks)
      res @?= V.fromList expected,
    testCase "mGetClusterRaw returns the value at every position of a duplicated key" $ do
      (env, runId) <- getCtx
      let a = mkKey runId "dup:a"
          b = mkKey runId "dup:b"
          c = mkKey runId "dup:c"
          ks = [a, b, a, c, b, a, c]
          val k = encodeUtf8 ("v-" <> k)
      res <- withKeys env [(k, val k) | k <- [a, b, c]] $ runTestM env (mGetClusterRaw ks)
      res @?= V.fromList (Just . val <$> ks),
    testCase "mGetClusterRaw splits a multi-key slot MGET back to the right indices" $ do
      (env, runId) <- getCtx
      let tagged = [mkKey runId ("{tag-" <> runId <> "}:" <> tshow i) | i <- [1 .. 40 :: Int]]
          untagged = [mkKey runId ("untagged:" <> tshow i) | i <- [1 .. 40 :: Int]]
          ks = concat [[t, u] | (t, u) <- zip (reverse tagged) untagged]
          val k = encodeUtf8 ("v-" <> k)
      assertEqual "tagged keys share one slot" 1 (distinctSlots tagged)
      res <- withKeys env [(k, val k) | k <- ks] $ runTestM env (mGetClusterRaw ks)
      res @?= V.fromList (Just . val <$> ks),
    testCase "mGetClusterRaw puts Nothing only at the index of a non-string key" $ do
      (env, runId) <- getCtx
      let a = mkKey runId "type:a"
          h = mkKey runId "type:hash"
          b = mkKey runId "type:b"
      void $ Hedis.runRedis (conn env) (Hedis.hset (encodeUtf8 h) "field" "x")
      res <- withKeys env [(a, "va"), (b, "vb")] (runTestM env (mGetClusterRaw [a, h, b])) `Exc.finally` deleteKeys env [h]
      res @?= V.fromList [Just "va", Nothing, Just "vb"],
    testCase "mGetClusterRaw returns an empty vector for no keys" $ do
      (env, _) <- getCtx
      res <- runTestM env (mGetClusterRaw [])
      res @?= V.empty,
    testCase "mGetClusterRaw stays aligned past the 1000-command pipeline flush (2500 keys)" $ do
      (env, runId) <- getCtx
      let ks = [mkKey runId ("big:" <> tshow i) | i <- [1 .. 2500 :: Int]]
          vs = [value i | i <- [1 .. 2500 :: Int]]
      res <- withKeys env (zip ks vs) $ runTestM env (mGetClusterRaw ks)
      res @?= V.fromList (Just <$> vs),
    testCase "mGetClusterWithKeys decodes values in key order and skips missing keys" $ do
      (env, runId) <- getCtx
      let ks = [mkKey runId ("json:" <> tshow i) | i <- [1 .. 50 :: Int]]
          present = [(k, i) | (i, k) <- zip [1 :: Int ..] ks, i `mod` 3 /= 0]
      res <- withKeys env [(k, BSL.toStrict (A.encode i)) | (k, i) <- present] $ runTestM env (mGetClusterWithKeys @Int ks)
      res @?= present,
    testCase "runPipelinedByKey returns one result per key in input order" $ do
      (env, runId) <- getCtx
      let a = mkKey runId "pipe:a"
          missing = mkKey runId "pipe:missing"
          h = mkKey runId "pipe:hash"
      void $ Hedis.runRedis (conn env) (Hedis.hset (encodeUtf8 h) "field" "x")
      res <- withKeys env [(a, "va")] (runTestM env (runPipelinedByKey "test" Hedis.get [a, missing, h])) `Exc.finally` deleteKeys env [h]
      res @?= [Just (Just "va"), Just Nothing, Nothing],
    testCase "zAddIfPossibleMany reserves per key in input order and respects the cap" $ do
      (env, runId) <- getCtx
      let ks = [mkKey runId ("zadd:" <> tshow i) | i <- [1 .. 50 :: Int]]
          atCap = [k | (i, k) <- zip [1 :: Int ..] ks, i `mod` 10 == 0]
          expected = [if i `mod` 10 == 0 then 0 else 1 | i <- [1 .. 50 :: Int]]
      (res, cards) <-
        ( do
            void $
              Hedis.runRedis (conn env) $ do
                rs <- mapM (\k -> Hedis.zadd (encodeUtf8 k) [(9.0e12, "other-a"), (9.0e12, "other-b")]) atCap
                liftIO (mapM_ Exc.evaluate rs)
            res <- runTestM env (zAddIfPossibleMany ks ("search-1", 9.0e12) 2 1.7e9)
            cards <- Hedis.runRedis (conn env) $ do
              rs <- mapM (Hedis.zcard . encodeUtf8) ks
              liftIO (mapM_ Exc.evaluate rs)
              pure rs
            pure (res, cards)
          )
          `Exc.finally` deleteKeys env ks
      res @?= expected
      cards @?= [Right (if i `mod` 10 == 0 then 2 else 1) | i <- [1 .. 50 :: Int]]
  ]

mkKey :: Text -> Text -> Text
mkKey runId suffix = "mobility-core-test:" <> runId <> ":" <> suffix

tshow :: Int -> Text
tshow = T.pack . P.show

value :: Int -> BS.ByteString
value i = BC.pack ("value-" <> P.show i)

conn :: TestEnv -> Hedis.Connection
conn env = env.hedisClusterEnv.hedisConnection

distinctSlots :: [Text] -> Int
distinctSlots = Set.size . Set.fromList . map (Hedis.keyToSlot . encodeUtf8)

withKeys :: TestEnv -> [(Text, BS.ByteString)] -> IO a -> IO a
withKeys env kvs action = do
  replies <- Hedis.runRedis (conn env) $ do
    rs <- mapM (\(k, v) -> Hedis.set (encodeUtf8 k) v) kvs
    liftIO (mapM_ Exc.evaluate rs)
    pure rs
  assertBool "seeding test keys failed" (all isRight replies)
  action `Exc.finally` deleteKeys env (fst <$> kvs)

deleteKeys :: TestEnv -> [Text] -> IO ()
deleteKeys env ks = void $
  Hedis.runRedis (conn env) $ do
    rs <- mapM (\k -> Hedis.del [encodeUtf8 k]) ks
    liftIO (mapM_ Exc.evaluate rs)
