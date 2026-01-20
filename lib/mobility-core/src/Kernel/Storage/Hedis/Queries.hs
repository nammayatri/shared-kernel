{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.Hedis.Queries (module Reexport, module Kernel.Storage.Hedis.Queries) where

import qualified Data.Aeson as Ae
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.String.Conversions
import Data.Text hiding (concatMap, map, null)
import qualified Data.Text as T
import qualified Data.Text as Text
import Database.Redis as Reexport (GeoBy (..), GeoFrom (..), Queued, Redis, RedisTx, Reply, TxResult (..))
import qualified Database.Redis as Hedis
import qualified Database.Redis.Cluster as Cluster
import EulerHS.Prelude (whenLeft)
import GHC.Records.Extra
import Kernel.Prelude
import Kernel.Storage.Hedis.Config
import Kernel.Storage.Hedis.Error
import Kernel.Types.Common
import Kernel.Utils.DatastoreLatencyCalculator
import qualified Kernel.Utils.Error.Throwing as Error
import Kernel.Utils.Logging
import qualified Test.RandomStrings as RS

type ExpirationTime = Int

data XReadResponse = XReadResponse
  { stream :: BS.ByteString,
    records :: [StreamsRecord]
  }
  deriving (Show)

data StreamsRecord = StreamsRecord
  { recordId :: BS.ByteString,
    keyValues :: [(BS.ByteString, BS.ByteString)]
  }
  deriving (Show)

convertFromHedisResponse :: Hedis.XReadResponse -> XReadResponse
convertFromHedisResponse hedisResponse =
  XReadResponse
    { stream = Hedis.stream hedisResponse,
      records = map convertFromHedisRecord (Hedis.records hedisResponse)
    }

decodeJSONWithErrorHandler ::
  (FromJSON a, HedisFlow m env, TryException m) =>
  Text ->
  BS.ByteString ->
  m () ->
  m (Maybe a)
decodeJSONWithErrorHandler key bs errorHandler = do
  case Ae.eitherDecode (BSL.fromStrict bs) of
    Left e -> do
      logTagError "REDIS" $ "Decode Failure for key:" <> key <> ", with value:" <> cs bs <> ", error:" <> cs e
      errorHandler
      pure Nothing
    Right a -> do
      pure $ Just a

convertFromHedisRecord :: Hedis.StreamsRecord -> StreamsRecord
convertFromHedisRecord hedisRecord =
  StreamsRecord
    { recordId = Hedis.recordId hedisRecord,
      keyValues = Hedis.keyValues hedisRecord
    }

runHedis ::
  (HedisFlow m env, TryException m) => Redis (Either Reply a) -> m a
runHedis action = do
  eithRes <- runHedisEither action
  Error.fromEitherM (HedisReplyError . show) eithRes

runHedisEither ::
  (HedisFlow m env, TryException m) => Redis (Either Reply a) -> m (Either Reply a)
runHedisEither action = do
  con <- asks (.hedisClusterEnv.hedisConnection)
  liftIO $ Hedis.runRedis con action

runHedisTransaction ::
  (HedisFlow m env, TryException m) => RedisTx (Queued a) -> m a
runHedisTransaction action = do
  con <- asks (.hedisClusterEnv.hedisConnection)
  res <- liftIO . Hedis.runRedis con $ Hedis.multiExec action
  case res of
    TxError err -> Error.throwError $ HedisReplyError err
    TxAborted -> Error.throwError HedisTransactionAborted
    TxSuccess a -> return a

----------------------------------------------------

modifyMasterOnlyConnection :: Hedis.Connection -> Hedis.Connection
modifyMasterOnlyConnection (Hedis.ClusteredConnection connInfo (Cluster.Connection mvar infoMap clusterCfg)) =
  Hedis.ClusteredConnection connInfo (Cluster.Connection mvar infoMap (clusterCfg {Cluster.useMasterOnly = Just True}))
modifyMasterOnlyConnection nonClustered = nonClustered -- If it's NonClusteredConnection, return it unchanged

withMasterRedis ::
  (HedisFlow m env, TryException m) => m f -> m f
withMasterRedis f = do
  local
    ( \env ->
        env{hedisEnv =
              (env.hedisEnv)
                { hedisConnection = modifyMasterOnlyConnection (hedisConnection (env.hedisEnv))
                },
            hedisClusterEnv =
              (env.hedisClusterEnv)
                { hedisConnection = modifyMasterOnlyConnection (hedisConnection (env.hedisClusterEnv))
                },
            hedisNonCriticalEnv =
              (env.hedisNonCriticalEnv)
                { hedisConnection = modifyMasterOnlyConnection (hedisConnection (env.hedisNonCriticalEnv))
                },
            hedisNonCriticalClusterEnv =
              (env.hedisNonCriticalClusterEnv)
                { hedisConnection = modifyMasterOnlyConnection (hedisConnection (env.hedisNonCriticalClusterEnv))
                }
           }
    )
    f

-- Just remove key modifier, so it won't modify the key with app prefixes
withCrossAppRedis ::
  (HedisFlow m env, TryException m) => m f -> m f
withCrossAppRedis f = do
  local (\env -> env{hedisEnv = env.hedisEnv{keyModifier = identity}, hedisClusterEnv = env.hedisClusterEnv{keyModifier = identity}}) f

withNonCriticalCrossAppRedis ::
  (HedisFlow m env, TryException m) => m f -> m f
withNonCriticalCrossAppRedis f = do
  local (\env -> env{hedisEnv = env.hedisNonCriticalEnv{keyModifier = identity}, hedisClusterEnv = env.hedisNonCriticalClusterEnv{keyModifier = identity}}) f

withNonCriticalRedis ::
  (HedisFlow m env, TryException m) => m f -> m f
withNonCriticalRedis f = do
  local (\env -> env{hedisEnv = env.hedisNonCriticalEnv, hedisClusterEnv = env.hedisNonCriticalClusterEnv}) f

-- Run the action on both primary and secondary Redis clusters (if secondary exists)
-- If isWriteOperation is True: writes to both primary and secondary (returns primary result)
-- If isWriteOperation is False: reads from primary first, falls back to secondary if primary returns Nothing
-- Returns the result from primary Redis; secondary Redis failures are logged but don't affect the result
runInMultiCloudRedis ::
  (HedisFlow m env, TryException m) => Bool -> m (Maybe a) -> m (Maybe a)
runInMultiCloudRedis isWriteOperation action = do
  mbSecondaryEnv <- asks (.secondaryHedisClusterEnv)
  case mbSecondaryEnv of
    Nothing -> action
    Just secondaryEnv
      | isWriteOperation -> do
        -- Write operation: write to both primary and secondary
        primaryResult <- action
        -- Run on secondary Redis, but don't fail if it errors - just log
        secondaryResult <-
          withTryCatch "runInMultiCloudRedis" $
            local (\env -> env{hedisEnv = secondaryEnv}) action
        case secondaryResult of
          Left err -> do
            logTagInfo "SECONDARY_CLUSTER:FAILED_TO_RUN_IN_SECONDARY_REDIS" $ show err
            pure primaryResult
          Right _ -> pure primaryResult
      | otherwise -> do
        -- Read operation: try primary first, fallback to secondary if Nothing
        primaryResult <- action
        case primaryResult of
          Just _ -> pure primaryResult -- Primary has result, return immediately
          Nothing -> do
            -- Primary returned Nothing, try secondary
            secondaryResult <-
              withTryCatch "runInMultiCloudRedis" $
                local (\env -> env{hedisEnv = secondaryEnv}) action
            case secondaryResult of
              Left err -> do
                logTagInfo "SECONDARY_CLUSTER:FAILED_TO_RUN_IN_SECONDARY_REDIS" $ show err
                pure Nothing
              Right result -> pure result

buildKey :: (HedisFlow m env, TryException m) => Text -> m BS.ByteString
buildKey key = do
  keyModifier <- asks (.hedisEnv.keyModifier)
  return . cs $ keyModifier key

runWithPrefixEither :: (HedisFlow m env, TryException m) => Text -> (BS.ByteString -> Redis (Either Reply a)) -> m (Either Reply a)
runWithPrefixEither key action = do
  prefKey <- buildKey key
  runHedisEither $ action prefKey

runWithPrefix :: (HedisFlow m env, TryException m) => Text -> (BS.ByteString -> Redis (Either Reply a)) -> m a
runWithPrefix key action = do
  prefKey <- buildKey key
  runHedis $ action prefKey

runWithPrefix_ :: (HedisFlow m env, TryException m) => Text -> (BS.ByteString -> Redis (Either Reply a)) -> m ()
runWithPrefix_ key action = void $ runWithPrefix key action

runWithPrefix'_ :: (HedisFlow m env, TryException m) => Text -> (BS.ByteString -> Redis (Either Reply a)) -> m ()
runWithPrefix'_ key action = void $ runWithPrefix' key action

runWithPrefix' :: (HedisFlow m env, TryException m) => Text -> (BS.ByteString -> Redis (Either Reply a)) -> m a
runWithPrefix' key action = do
  prefKey <- buildKey key
  runHedis' $ action prefKey

runHedisTransaction' ::
  (HedisFlow m env, TryException m) => RedisTx (Queued a) -> m a
runHedisTransaction' action = do
  con <- asks (.hedisEnv.hedisConnection)
  res <- liftIO . Hedis.runRedis con $ Hedis.multiExec action
  case res of
    TxError err -> Error.throwError $ HedisReplyError err
    TxAborted -> Error.throwError HedisTransactionAborted
    TxSuccess a -> return a

runHedis' ::
  (HedisFlow m env, TryException m) => Redis (Either Reply a) -> m a
runHedis' action = do
  eithRes <- runHedisEither' action
  Error.fromEitherM (HedisReplyError . show) eithRes

runHedisEither' ::
  (HedisFlow m env, TryException m) => Redis (Either Reply a) -> m (Either Reply a)
runHedisEither' action = do
  con <- asks (.hedisEnv.hedisConnection)
  liftIO $ Hedis.runRedis con action

tryGetFromStandalone :: (HedisFlow m env, TryException m) => Text -> m (Maybe BS.ByteString)
tryGetFromStandalone key = withLogTag "STANDALONE" $ do
  eitherMaybeBS <- withTimeRedis "RedisStandalone" "get" $ withTryCatch "tryGetFromStandalone" (runWithPrefix' key Hedis.get)
  case eitherMaybeBS of
    Left err -> logTagInfo "ERROR_WHILE_GET" (show err) $> Nothing
    Right maybeBS -> pure maybeBS

tryGetFromCluster :: (HedisFlow m env, TryException m) => Text -> m (Maybe BS.ByteString)
tryGetFromCluster key = withLogTag "CLUSTER" $ do
  eitherMaybeBS <- withTimeRedis "RedisCluster" "get" $ withTryCatch "tryGetFromCluster" (runWithPrefix key Hedis.get)
  case eitherMaybeBS of
    Left err -> logTagInfo "ERROR_WHILE_GET" (show err) $> Nothing
    Right maybeBS -> pure maybeBS

getImpl :: (FromJSON a, HedisFlow m env, TryException m) => (BS.ByteString -> m (Maybe a)) -> Text -> m (Maybe a)
getImpl decodeResult key = withLogTag "Redis" $ do
  res <- tryGetFromCluster key
  case res of
    Nothing -> do
      migrating <- asks (.hedisMigrationStage)
      if migrating
        then do
          res' <- tryGetFromStandalone key
          maybe (pure Nothing) decodeResult res'
        else pure Nothing
    Just res' -> decodeResult res'

get :: (FromJSON a, HedisFlow m env, TryException m) => Text -> m (Maybe a)
get = safeGet

get' ::
  (FromJSON a, HedisFlow m env, TryException m) => Text -> m () -> m (Maybe a)
get' key decodeErrHandler = getImpl decodeResult key
  where
    decodeResult bs = decodeJSONWithErrorHandler key bs decodeErrHandler

safeGet ::
  (FromJSON a, HedisFlow m env, TryException m) => Text -> m (Maybe a)
safeGet key = get' key (del key)

set ::
  (ToJSON a, HedisFlow m env, TryException m) => Text -> a -> m ()
set key val = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      res <- withTimeRedis "RedisStandalone" "set" $ withTryCatch "set" (runWithPrefix'_ key $ \prefKey -> Hedis.set prefKey $ BSL.toStrict $ Ae.encode val)
      whenLeft res (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_SET" $ show err)
    else pure ()
  res <- withTimeRedis "RedisCluster" "set" $ withTryCatch "set" (runWithPrefix_ key $ \prefKey -> Hedis.set prefKey $ BSL.toStrict $ Ae.encode val)
  whenLeft res (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_SET" $ show err)

setExp ::
  (ToJSON a, HedisFlow m env, TryException m) => Text -> a -> ExpirationTime -> m ()
setExp key val expirationTime = withTimeRedis "Redis" "setExp" . withLogTag "Redis" $ do
  prefKey <- buildKey key
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      standaloneRes <-
        withTimeRedis "RedisStandalone" "setExp" $
          withTryCatch "setExp" $ do
            void . runHedisTransaction' $ do
              void . Hedis.set prefKey $ BSL.toStrict $ Ae.encode val
              Hedis.expire prefKey (toInteger expirationTime)
      whenLeft standaloneRes (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_SETEXP" $ show err)
    else pure ()
  clusterRes <-
    withTimeRedis "RedisCluster" "setExp" $
      withTryCatch "setExp" $ do
        void . runHedisTransaction $ do
          void . Hedis.set prefKey $ BSL.toStrict $ Ae.encode val
          Hedis.expire prefKey (toInteger expirationTime)
  whenLeft clusterRes (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_SETEXP" $ show err)

setNx ::
  (ToJSON a, HedisFlow m env, TryException m) => Text -> a -> m Bool
setNx key val = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  writtenInStandalone <-
    if migrating
      then do
        standaloneRes <- withTimeRedis "RedisStandalone" "setNx" $ withTryCatch "setNx" $ runWithPrefix' key $ \prefKey -> Hedis.setnx prefKey $ BSL.toStrict $ Ae.encode val
        case standaloneRes of
          Left err -> withLogTag "STANDALONE" (logTagInfo "FAILED_TO_SETNX" $ show err) $> False
          Right res -> pure res
      else pure False
  clusterRes <- withTimeRedis "RedisCluster" "setNx" $ withTryCatch "setNx" $ runWithPrefix key $ \prefKey -> Hedis.setnx prefKey $ BSL.toStrict $ Ae.encode val
  case clusterRes of
    Left err -> withLogTag "CLUSTER" (logTagInfo "FAILED_TO_SETNX" $ show err) $> (writtenInStandalone || False)
    Right res -> pure $ writtenInStandalone || res

del :: (HedisFlow m env, TryException m) => Text -> m ()
del key = withLogTag "Redis" do
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      res <- withTimeRedis "RedisStandalone" "del" $ withTryCatch "del" (runWithPrefix'_ key $ \prefKey -> Hedis.del [prefKey])
      whenLeft res (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_DELETE" $ show err)
    else pure ()
  res <- withTimeRedis "RedisCluster" "del" $ withTryCatch "del" (runWithPrefix_ key $ \prefKey -> Hedis.del [prefKey])
  whenLeft res (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_DELETE" $ show err)

rPushExp :: (HedisFlow m env, TryException m, ToJSON a) => Text -> [a] -> ExpirationTime -> m ()
rPushExp key list ex = withLogTag "Redis" $ do
  prefKey <- buildKey key
  unless (null list) $ do
    migrating <- asks (.hedisMigrationStage)
    if migrating
      then do
        standaloneRes <-
          withTimeRedis "RedisStandalone" "rPushExp" $ do
            withTryCatch "rPushExp" $ do
              void . runHedisTransaction' $ do
                void . Hedis.rpush prefKey $ map (BSL.toStrict . Ae.encode) list
                Hedis.expire prefKey (toInteger ex)
        whenLeft standaloneRes (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_PUSHEXP" $ show err)
      else pure ()
    clusterRes <-
      withTimeRedis "RedisCluster" "rPushExp" $ do
        withTryCatch "rPushExp" $ do
          void . runHedisTransaction $ do
            void . Hedis.rpush prefKey $ map (BSL.toStrict . Ae.encode) list
            Hedis.expire prefKey (toInteger ex)
    whenLeft clusterRes (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_PUSHEXP" $ show err)

lPush :: (HedisFlow m env, TryException m, ToJSON a) => Text -> NonEmpty a -> m ()
lPush key list = withTimeRedis "RedisCluster" "lPush" . runWithPrefix_ key $ \prefKey ->
  Hedis.lpush prefKey $ map (BSL.toStrict . Ae.encode) (toList list)

rPush :: (HedisFlow m env, TryException m, ToJSON a) => Text -> NonEmpty a -> m ()
rPush key list = withTimeRedis "RedisCluster" "rPush" . runWithPrefix_ key $ \prefKey ->
  Hedis.rpush prefKey $ map (BSL.toStrict . Ae.encode) (toList list)

rPop :: (HedisFlow m env, TryException m, FromJSON a) => Text -> m (Maybe a)
rPop key = withTimeRedis "RedisCluster" "rPop" $ do
  res <- runWithPrefix key $ \prefKey -> Hedis.rpop prefKey
  pure $ Ae.decode . BSL.fromStrict =<< res

lTrim :: (HedisFlow m env, TryException m) => Text -> Integer -> Integer -> m ()
lTrim key start stop = withTimeRedis "RedisCluster" "lTrim" . runWithPrefix_ key $ \prefKey ->
  Hedis.ltrim prefKey start stop

clearList :: (HedisFlow m env, TryException m) => Text -> m ()
clearList key = lTrim key 2 1

lLen :: (HedisFlow m env, TryException m) => Text -> m Integer
lLen key = withTimeRedis "RedisCluster" "lLen" $ runWithPrefix key Hedis.llen

lRange :: (HedisFlow m env, TryException m, FromJSON a) => Text -> Integer -> Integer -> m [a]
lRange key start stop = withTimeRedis "RedisCluster" "lRange" $ do
  res <- runWithPrefix key $ \prefKey ->
    Hedis.lrange prefKey start stop
  mapM (\a -> Error.fromMaybeM (HedisDecodeError $ cs a) . Ae.decode $ cs a) res

getList :: (HedisFlow m env, TryException m, FromJSON a) => Text -> m [a]
getList key = lRange key 0 (-1)

incr :: (HedisFlow m env, TryException m) => Text -> m Integer
incr key = withTimeRedis "RedisCluster" "incr" $ runWithPrefix key Hedis.incr

incrby :: (HedisFlow m env, TryException m) => Text -> Integer -> m Integer
incrby key val = withTimeRedis "RedisCluster" "incrBy" $ runWithPrefix key $ flip Hedis.incrby val

decr :: (HedisFlow m env, TryException m) => Text -> m Integer
decr key = withTimeRedis "RedisCluster" "decr" $ runWithPrefix key Hedis.decr

decrby :: (HedisFlow m env, TryException m) => Text -> Integer -> m Integer
decrby key val = withTimeRedis "RedisCluster" "decrBy" $ runWithPrefix key $ flip Hedis.decrby val

incrByFloat :: (HedisFlow m env, TryException m) => Text -> Double -> m Double
incrByFloat key toAdd = withTimeRedis "RedisCluster" "incrByFloat" . runWithPrefix key $ \prefKey ->
  Hedis.incrbyfloat prefKey toAdd

expire :: (HedisFlow m env, TryException m) => Text -> ExpirationTime -> m ()
expire key expirationTime = do
  migrating <- asks (.hedisMigrationStage)
  when migrating . withTimeRedis "RedisStandalone" "expire" . runWithPrefix'_ key $ \prefKey -> Hedis.expire prefKey (toInteger expirationTime)
  withTimeRedis "RedisCluster" "expire" . runWithPrefix_ key $ \prefKey -> Hedis.expire prefKey (toInteger expirationTime)

setNxExpire :: (ToJSON a, HedisFlow m env, TryException m) => Text -> ExpirationTime -> a -> m Bool
setNxExpire key expirationTime val = withTimeRedis "RedisCluster" "setNxExpire" $ do
  eithRes <- runWithPrefixEither key $ \prefKey ->
    Hedis.setOpts prefKey (cs $ Ae.encode val) $
      Hedis.SetOpts (Just $ toInteger expirationTime) Nothing (Just Hedis.Nx)
  pure $ case eithRes of
    Right Hedis.Ok -> True
    _ -> False

decrIfExist :: (HedisFlow m env, TryException m) => Text -> m Integer
decrIfExist key = do
  res <- withTimeRedis "RedisCluster" "decrIfExist" $
    runWithPrefix key $ \prefKey ->
      Hedis.eval
        "if tonumber(redis.call('GET', KEYS[1]) or '0') > 0 then return redis.call('DECR', KEYS[1]) else return nil end"
        [prefKey]
        []
  case res of
    Just (Hedis.Integer i) -> pure i
    _ -> pure (-1)

zAddIfPossible :: (HedisFlow m env, TryException m) => Text -> (Text, Double) -> Int -> Double -> m Integer
zAddIfPossible key (member, score) maxSize fromScore = do
  let script =
        "if redis.call('ZCOUNT', KEYS[1], ARGV[1], ARGV[2]) < tonumber(ARGV[3]) then "
          ++ "redis.call('ZADD', KEYS[1], ARGV[2], ARGV[4]); return 1 "
          ++ "else return 0 end"
  result <- withTimeRedis "RedisCluster" "zAddIfPossible" $
    runWithPrefix key $ \prefKey ->
      Hedis.eval (cs script) [prefKey] [cs (show fromScore :: String), cs (show score :: String), cs (show maxSize :: String), cs member]

  case result of
    Just (Hedis.Integer i) -> pure i
    _ -> pure (-1)

tryLockRedis :: (HedisFlow m env, TryException m) => Text -> ExpirationTime -> m Bool
tryLockRedis key timeout = setNxExpire (buildLockResourceName key) timeout ()

unlockRedis :: (HedisFlow m env, TryException m) => Text -> m ()
unlockRedis key = void . del $ buildLockResourceName key

whenWithLockRedis :: (HedisFlow m env, TryException m, MonadMask m) => Text -> ExpirationTime -> m () -> m ()
whenWithLockRedis key timeout func = do
  whenM (tryLockRedis key timeout) $ do
    finally func $ unlockRedis key

whenWithLockRedisAndReturnValue :: (HedisFlow m env, TryException m, MonadMask m) => Text -> ExpirationTime -> m a -> m (Either () a)
whenWithLockRedisAndReturnValue key timeout func = do
  locked <- tryLockRedis key timeout
  if locked
    then do
      res <- finally func $ unlockRedis key
      return $ Right res
    else return $ Left ()

withWaitAndLockRedis :: (HedisFlow m env, TryException m, MonadMask m) => Text -> ExpirationTime -> Int -> m a -> m a
withWaitAndLockRedis key timeout delay func = do
  getLock
  finally func (unlockRedis key)
  where
    getLock = do
      lockAvailable <- tryLockRedis key timeout
      threadDelay delay
      unless lockAvailable getLock

withWaitAndLockCrossAppRedis :: (HedisFlow m env, TryException m, MonadMask m) => Text -> ExpirationTime -> Int -> m a -> m a
withWaitAndLockCrossAppRedis key timeout delay func = do
  withCrossAppRedis getLock
  finally func (withCrossAppRedis $ unlockRedis key)
  where
    getLock = do
      lockAvailable <- tryLockRedis key timeout
      threadDelay delay
      unless lockAvailable getLock

withLockRedis :: (HedisFlow m env, TryException m, MonadMask m) => Text -> ExpirationTime -> m () -> m ()
withLockRedis key timeout func = do
  getLock
  finally func (unlockRedis key)
  where
    getLock = do
      lockAvailable <- tryLockRedis key timeout
      unless lockAvailable getLock

withLockRedisAndReturnValue :: (HedisFlow m env, TryException m, MonadMask m) => Text -> ExpirationTime -> m a -> m a
withLockRedisAndReturnValue key timeout func = do
  getLock
  finally func (unlockRedis key)
  where
    getLock = do
      lockAvailable <- tryLockRedis key timeout
      unless lockAvailable getLock

withWaitOnLockRedisWithExpiry :: (HedisFlow m env, TryException m, MonadMask m) => Text -> ExpirationTime -> ExpirationTime -> m () -> m ()
withWaitOnLockRedisWithExpiry key timeout recursionTimeOut func = do
  uuid <- T.pack <$> liftIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)
  let keyE = "recursion timeout for:" <> uuid
  setExp keyE True recursionTimeOut
  withMasterRedis $ withWaitOnLockRedisWithExpiry' keyE key timeout func

withWaitOnLockRedisWithExpiry' :: (HedisFlow m env, TryException m, MonadMask m) => Text -> Text -> ExpirationTime -> m () -> m ()
withWaitOnLockRedisWithExpiry' recursionTimedOutKey key timeout func = do
  toExecute <- getLock recursionTimedOutKey
  when toExecute $ do
    finally func $ do
      unlockRedis key
      del recursionTimedOutKey
  where
    getLock recurrsionTimedOutKey' = do
      get recurrsionTimedOutKey' >>= \case
        Just a -> do
          lockAvailable <- tryLockRedis key timeout
          if not lockAvailable && a
            then getLock recurrsionTimedOutKey'
            else return True
        Nothing -> do
          tryLockRedis key timeout

buildLockResourceName :: (IsString a) => Text -> a
buildLockResourceName key = fromString $ "mobility:locker:" <> Text.unpack key

hSetExp :: (ToJSON a, HedisFlow m env, TryException m) => Text -> Text -> a -> ExpirationTime -> m ()
hSetExp key field value expirationTime = withLogTag "Redis" $ do
  prefKey <- buildKey key
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      standaloneRes <-
        withTimeRedis "RedisStandalone" "hSetExp" $ do
          withTryCatch "hSetExp" $ do
            void . runHedisTransaction' $ do
              void . Hedis.hset prefKey (cs field) $ BSL.toStrict $ Ae.encode value
              Hedis.expire prefKey (toInteger expirationTime)
      whenLeft standaloneRes (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_HSETEXP" $ show err)
    else pure ()
  clusterRes <-
    withTimeRedis "RedisCluster" "hSetExp" $ do
      withTryCatch "hSetExp" $ do
        void . runHedisTransaction $ do
          void . Hedis.hset prefKey (cs field) $ BSL.toStrict $ Ae.encode value
          Hedis.expire prefKey (toInteger expirationTime)
  whenLeft clusterRes (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_HSETEXP" $ show err)

safeHGet :: (FromJSON a, HedisFlow m env, TryException m) => Text -> Text -> m (Maybe a)
safeHGet key field =
  withTimeRedis "RedisCluster" "safeHGet" $ do
    maybeBS <- runWithPrefix key (`Hedis.hget` cs field)
    case maybeBS of
      Nothing -> pure Nothing
      Just bs -> decodeJSONWithErrorHandler key bs (hDel key [field])

hGet :: (FromJSON a, HedisFlow m env, TryException m) => Text -> Text -> m (Maybe a)
hGet key field =
  withTimeRedis "RedisCluster" "hGet" $ do
    maybeBS <- runWithPrefix key (`Hedis.hget` cs field)
    case maybeBS of
      Nothing -> pure Nothing
      Just bs -> Error.fromMaybeM (HedisDecodeError $ cs bs) $ Ae.decode $ BSL.fromStrict bs

hmGet :: (FromJSON a, HedisFlow m env, TryException m) => Text -> [Text] -> m [Maybe a]
hmGet key fields =
  withTimeRedis "RedisCluster" "hmGet" $ do
    listBS <- runWithPrefix key (`Hedis.hmget` map cs fields)
    mapM decodeBS listBS
  where
    decodeBS :: (FromJSON a, HedisFlow m env, TryException m) => Maybe BS.ByteString -> m (Maybe a)
    decodeBS Nothing = pure Nothing
    decodeBS (Just bs) = Error.fromMaybeM (HedisDecodeError $ cs bs) $ Ae.decode $ BSL.fromStrict bs

hDel :: (HedisFlow m env, TryException m) => Text -> [Text] -> m ()
hDel key fields = withTimeRedis "RedisCluster" "hDel" $ runWithPrefix_ key (`Hedis.hdel` map cs fields)

hGetAll :: (FromJSON a, HedisFlow m env, TryException m) => Text -> m [(Text, a)]
hGetAll key = withTimeRedis "RedisCluster" "hGetAll" $ do
  hMap <- runWithPrefix key Hedis.hgetall
  pure $ mapMaybe (\(k, val) -> (cs k,) <$> Ae.decode (BSL.fromStrict val)) hMap

zAddExp :: (ToJSON Integer, HedisFlow m env, TryException m) => Text -> Text -> Integer -> ExpirationTime -> m ()
zAddExp key field value expirationTime = withLogTag "Redis" $ do
  prefKey <- buildKey key
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      standaloneRes <-
        withTryCatch "zAddExp" $ do
          void . runHedisTransaction' $ do
            void . Hedis.zadd prefKey $ [(fromIntegral value, cs field)]
            Hedis.expire prefKey (toInteger expirationTime)
      whenLeft standaloneRes (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_ZADDEXP" $ show err)
    else pure ()
  clusterRes <-
    withTryCatch "zAddExp" $ do
      void . runHedisTransaction $ do
        void . Hedis.zadd prefKey $ [(fromIntegral value, cs field)]
        Hedis.expire prefKey (toInteger expirationTime)
  whenLeft clusterRes (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZADDEXP" $ show err)

zrevrangeWithscores :: (HedisFlow m env, TryException m) => Text -> Integer -> Integer -> m [(Text, Double)]
zrevrangeWithscores key start stop = do
  res <- runWithPrefix key $ \prefKey ->
    Hedis.zrevrangeWithscores prefKey start stop
  pure $ map (\(k, score) -> (cs' k, score)) res
  where
    cs' :: BS.ByteString -> Text
    cs' = cs

zScore :: (FromJSON Double, HedisFlow m env, TryException m) => Text -> Text -> m (Maybe Double)
zScore key member = do
  maybeZS <- runWithPrefix key (`Hedis.zscore` cs member)
  pure maybeZS

zRevRank :: (FromJSON Integer, HedisFlow m env, TryException m) => Text -> Text -> m (Maybe Integer)
zRevRank key member = do
  maybeZr <- runWithPrefix key (`Hedis.zrevrank` cs member)
  pure maybeZr

zCard :: (HedisFlow m env, TryException m) => Text -> m Integer
zCard key = runWithPrefix key Hedis.zcard

zAdd ::
  (ToJSON member, HedisFlow m env, TryException m) =>
  Text ->
  [(Double, member)] ->
  m ()
zAdd key members = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      res <- withTimeRedis "RedisStandalone" "zAdd" $ withTryCatch "zAdd" (runWithPrefix'_ key $ \prefKey -> Hedis.zadd prefKey $ map (\(score, member) -> (score, BSL.toStrict $ Ae.encode member)) members)
      whenLeft res (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_ZADD" $ show err)
    else pure ()
  res <- withTimeRedis "RedisCluster" "zAdd" $ withTryCatch "zAdd" (runWithPrefix_ key $ \prefKey -> Hedis.zadd prefKey $ map (\(score, member) -> (score, BSL.toStrict $ Ae.encode member)) members)
  whenLeft res (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZADD" $ show err)

zCount :: (HedisFlow m env, TryException m) => Text -> Double -> Double -> m Integer
zCount key mn mx = withTimeRedis "RedisStandalone" "zCount" $ runWithPrefix key $ \prefKey -> Hedis.zcount prefKey mn mx

zIncrBy ::
  (ToJSON member, HedisFlow m env, TryException m) =>
  Text ->
  Integer ->
  member ->
  m ()
zIncrBy key increment member = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      res <- withTimeRedis "RedisStandalone" "zIncrBy" $ withTryCatch "zIncrBy" (runWithPrefix'_ key $ \prefKey -> Hedis.zincrby prefKey increment (BSL.toStrict $ Ae.encode member))
      whenLeft res (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_ZINCRBY" $ show err)
    else pure ()
  res <- withTimeRedis "RedisCluster" "zIncrBy" $ withTryCatch "zIncrBy" (runWithPrefix_ key $ \prefKey -> Hedis.zincrby prefKey increment (BSL.toStrict $ Ae.encode member))
  whenLeft res (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZINCRBY" $ show err)

xInfoGroups ::
  (HedisFlow m env, TryException m) =>
  Text -> -- Stream key
  m Bool
xInfoGroups key = do
  migrating <- asks (.hedisMigrationStage)
  eitherMaybeBS <-
    if migrating
      then withTimeRedis "RedisStandalone" "xInfoGroups" $ withTryCatch "xInfoGroups" (runWithPrefix' key Hedis.xinfoGroups)
      else withTimeRedis "RedisCluster" "xInfoGroups" $ withTryCatch "xInfoGroups" (runWithPrefix key Hedis.xinfoGroups)
  ls <-
    case eitherMaybeBS of
      Left err -> logTagInfo "ERROR_WHILE_GET_XInfoGroups" (show err) $> []
      Right maybeBS -> pure maybeBS
  return $ not (null ls)

-- Function to create a new consumer group for a stream
xGroupCreate ::
  (HedisFlow m env, TryException m) =>
  Text -> -- Stream key
  Text -> -- Group name
  Text -> -- Start ID
  m ()
xGroupCreate key groupName startId = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "xGroupCreate" $ withTryCatch "xGroupCreate" (runWithPrefix'_ key $ \prefKey -> Hedis.xgroupCreate prefKey (cs groupName) (cs startId))
    whenLeft res (withLogTag "STANDALONE" . logTagInfo "FAILED_TO_xGroupCreate" . show)
  res <- withTimeRedis "RedisCluster" "xGroupCreate" $ withTryCatch "xGroupCreate" (runWithPrefix_ key $ \prefKey -> Hedis.xgroupCreate prefKey (cs groupName) (cs startId))
  whenLeft res (withLogTag "CLUSTER" . logTagInfo "FAILED_TO_xGroupCreate" . show)

extractKeyValuePairs :: [StreamsRecord] -> [(Text, Text)]
extractKeyValuePairs = concatMap (\(StreamsRecord _ keyVals) -> map (\(k, v) -> (cs k, cs v)) keyVals)

extractRecordIds :: [StreamsRecord] -> [BS.ByteString]
extractRecordIds = map (\(StreamsRecord recordId _) -> recordId)

xReadGroup ::
  (HedisFlow m env, TryException m) =>
  Text -> -- group name
  Text -> -- consumer name
  [(Text, Text)] -> -- (stream, id) pairs
  m (Maybe [XReadResponse])
xReadGroup groupName consumerName pairsList = do
  let bsPairsList = map (\(stream, id) -> (cs stream, cs id)) pairsList
  let mbKeyVal = listToMaybe bsPairsList
  case mbKeyVal of
    Just keyVal -> do
      migrating <- asks (.hedisMigrationStage)
      eitherMaybeBS <-
        if migrating
          then withTimeRedis "RedisStandalone" "XReadGroup" $ withTryCatch "xReadGroup" (runWithPrefix' (cs $ fst keyVal) $ \_ -> Hedis.xreadGroup (cs groupName) (cs consumerName) bsPairsList)
          else withTimeRedis "RedisCluster" "XReadGroup" $ withTryCatch "xReadGroup" (runWithPrefix (cs $ fst keyVal) $ \_ -> Hedis.xreadGroup (cs groupName) (cs consumerName) bsPairsList)
      mbRes <-
        case eitherMaybeBS of
          Left err -> logTagInfo "ERROR_WHILE_GET_XReadGroup" (show err) $> Nothing
          Right maybeBS -> pure maybeBS
      case mbRes of
        Just res -> return $ Just (map convertFromHedisResponse res)
        Nothing -> pure Nothing
    Nothing -> pure Nothing

xReadGroupOpts ::
  (HedisFlow m env, TryException m) =>
  Text -> -- group name
  Text -> -- consumer name
  [(Text, Text)] -> -- (stream, id) pairs
  Maybe Integer ->
  Maybe Integer ->
  m (Maybe [XReadResponse])
xReadGroupOpts groupName consumerName pairsList block_ recordCount_ = do
  let opts = Hedis.XReadOpts {block = block_, recordCount = recordCount_, noack = False}
  let bsPairsList = map (\(stream, id) -> (cs stream, cs id)) pairsList
  let mbKeyVal = listToMaybe bsPairsList
  case mbKeyVal of
    Just keyVal -> do
      migrating <- asks (.hedisMigrationStage)
      eitherMaybeBS <-
        if migrating
          then withTimeRedis "RedisStandalone" "xReadGroupOpts" $ withTryCatch "xReadGroupOpts" (runWithPrefix' (cs $ fst keyVal) $ \_ -> Hedis.xreadGroupOpts (cs groupName) (cs consumerName) bsPairsList opts)
          else withTimeRedis "RedisCluster" "xReadGroupOpts" $ withTryCatch "xReadGroupOpts" (runWithPrefix (cs $ fst keyVal) $ \_ -> Hedis.xreadGroupOpts (cs groupName) (cs consumerName) bsPairsList opts)
      mbRes <-
        case eitherMaybeBS of
          Left err -> logTagInfo "ERROR_WHILE_GET_XReadGroupOpts" (show err) $> Nothing
          Right maybeBS -> pure maybeBS
      case mbRes of
        Just res -> return $ Just (map convertFromHedisResponse res)
        Nothing -> pure Nothing
    Nothing -> pure Nothing

xAdd :: (HedisFlow m env, TryException m) => Text -> Text -> [(BS.ByteString, BS.ByteString)] -> m BS.ByteString
xAdd key entryId fieldValues = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "xadd" $ withTryCatch "xAdd" (runWithPrefix'_ key $ \prefKey -> Hedis.xadd prefKey (cs entryId) fieldValues)
    whenLeft res (withLogTag "STANDALONE" . logTagInfo "FAILED_TO_xadd" . show)
  res <- withTimeRedis "RedisCluster" "xadd" $ withTryCatch "xAdd" (runWithPrefix key $ \prefKey -> Hedis.xadd prefKey (cs entryId) fieldValues)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "xadd" $ show err
      pure ""
    Right items -> pure items

xAddExp :: (HedisFlow m env, TryException m) => Text -> Text -> [(BS.ByteString, BS.ByteString)] -> ExpirationTime -> m ()
xAddExp key entryId fieldValues expirationTime = withLogTag "Redis" $ do
  prefKey <- buildKey key
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <-
      withTimeRedis "RedisStandalone" "xaddExp" $
        withTryCatch "xAddExp" $ do
          void $
            runHedisTransaction' $ do
              void $ Hedis.xadd prefKey (cs entryId) fieldValues
              Hedis.expire prefKey (toInteger expirationTime)
    whenLeft res (withLogTag "STANDALONE" . logTagInfo "FAILED_TO_xaddExp" . show)
  clusterRes <-
    withTimeRedis "RedisCluster" "xaddExp" $
      withTryCatch "xAddExp" $ do
        void $
          runHedisTransaction $ do
            void $ Hedis.xadd prefKey (cs entryId) fieldValues
            Hedis.expire prefKey (toInteger expirationTime)
  whenLeft clusterRes (withLogTag "CLUSTER" . logTagInfo "FAILED_TO_XADDEXP" . show)

zRangeByScoreByCount :: (HedisFlow m env, TryException m) => Text -> Double -> Double -> Integer -> Integer -> m [BS.ByteString]
zRangeByScoreByCount key start end offset limit = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "zRangeByScoreByCount" $ withTryCatch "zRangeByScoreByCount" (runWithPrefix'_ key $ \prefKey -> Hedis.zrangebyscoreLimit prefKey start end offset limit)
    whenLeft res (withLogTag "STANDALONE" . logTagInfo "FAILED_TO_ZRANGEBYSCOREBYCOUNT" . show)
  res <- withTimeRedis "RedisCluster" "zRangeByScoreByCount" $ withTryCatch "zRangeByScoreByCount" (runWithPrefix key $ \prefKey -> Hedis.zrangebyscoreLimit prefKey start end offset limit)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZRANGEBYSCOREBYCOUNT" $ show err
      pure [] -- Return an empty list if there was an error
    Right items -> pure items

zRangeByScore :: (HedisFlow m env, TryException m) => Text -> Double -> Double -> m [BS.ByteString]
zRangeByScore key start end = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "zRangeByScore" $ withTryCatch "zRangeByScore" (runWithPrefix'_ key $ \prefKey -> Hedis.zrangebyscore prefKey start end)
    whenLeft res (withLogTag "STANDALONE" . logTagInfo "FAILED_TO_ZRANGEBYSCORE" . show)
  res <- withTimeRedis "RedisCluster" "zRangeByScore" $ withTryCatch "zRangeByScore" (runWithPrefix key $ \prefKey -> Hedis.zrangebyscore prefKey start end)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZRANGEBYSCORE" $ show err
      pure [] -- Return an empty list if there was an error
    Right items -> pure items

zRange :: (HedisFlow m env, TryException m) => Text -> Integer -> Integer -> m [BS.ByteString]
zRange key start end = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "zRange" $ withTryCatch "zRange" (runWithPrefix'_ key $ \prefKey -> Hedis.zrange prefKey start end)
    whenLeft res (withLogTag "STANDALONE" . logTagInfo "FAILED_TO_ZRANGE" . show)
  res <- withTimeRedis "RedisCluster" "zRange" $ withTryCatch "zRange" (runWithPrefix key $ \prefKey -> Hedis.zrange prefKey start end)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZRANGE" $ show err
      pure [] -- Return an empty list if there was an error
    Right items -> pure items

zRangeWithScores :: (HedisFlow m env, TryException m) => Text -> Integer -> Integer -> m [(BS.ByteString, Double)]
zRangeWithScores key start end = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "zRangeWithScores" $ withTryCatch "zRangeWithScores" (runWithPrefix'_ key $ \prefKey -> Hedis.zrangeWithscores prefKey start end)
    whenLeft res (withLogTag "STANDALONE" . logTagInfo "FAILED_TO_ZRANGE_WITH_SCORES" . show)
  res <- withTimeRedis "RedisCluster" "zRangeWithScores" $ withTryCatch "zRangeWithScores" (runWithPrefix key $ \prefKey -> Hedis.zrangeWithscores prefKey start end)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZRANGE_WITH_SCORES" $ show err
      pure [] -- Return an empty list if there was an error
    Right items -> pure items

zRemRangeByScore :: (HedisFlow m env, TryException m) => Text -> Double -> Double -> m Integer
zRemRangeByScore key start end = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "zRemRangeByScore" $ withTryCatch "zRemRangeByScore" (runWithPrefix'_ key $ \prefKey -> Hedis.zremrangebyscore prefKey start end)
    case res of
      Left err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_ZREMRANGEBYSCORE" $ show err
      Right items -> pure items
  res <- withTimeRedis "RedisCluster" "zRemRangeByScore" $ withTryCatch "zRemRangeByScore" (runWithPrefix key $ \prefKey -> Hedis.zremrangebyscore prefKey start end)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZREMRANGEBYSCORE" $ show err
      pure (-1) -- Return -1 if there was an error
    Right items -> pure items

zRem :: (HedisFlow m env, TryException m) => Text -> [Text] -> m Integer
zRem key members = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "zRem" $ withTryCatch "zRem" (runWithPrefix'_ key $ \prefKey -> Hedis.zrem prefKey (map cs members))
    case res of
      Left err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_ZREM" $ show err
      Right items -> pure items
  res <- withTimeRedis "RedisCluster" "zRem" $ withTryCatch "zRem" (runWithPrefix key $ \prefKey -> Hedis.zrem prefKey (map cs members))
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZREM" $ show err
      pure (-1) -- Return -1 if there was an error
    Right items -> pure items

zRem' :: (ToJSON member, HedisFlow m env, TryException m) => Text -> [member] -> m ()
zRem' key members = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      res <- withTimeRedis "RedisStandalone" "zRem" $ withTryCatch "zRem'" (runWithPrefix'_ key $ \prefKey -> Hedis.zrem prefKey $ map (\val -> BSL.toStrict $ Ae.encode val) members)
      whenLeft res (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_ZREM" $ show err)
    else pure ()
  res <- withTimeRedis "RedisCluster" "zRem" $ withTryCatch "zRem'" (runWithPrefix_ key $ \prefKey -> Hedis.zrem prefKey $ map (\val -> BSL.toStrict $ Ae.encode val) members)
  whenLeft res (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZREM" $ show err)

xDel :: (HedisFlow m env, TryException m) => Text -> [BS.ByteString] -> m Integer
xDel key entryId = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "xDel" $ withTryCatch "xDel" (runWithPrefix'_ key $ \prefKey -> Hedis.xdel prefKey entryId)
    case res of
      Left err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_XDEL" $ show err
      Right items -> pure items
  res <- withTimeRedis "RedisCluster" "xDel" $ withTryCatch "xDel" (runWithPrefix key $ \prefKey -> Hedis.xdel prefKey entryId)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_XDEL" $ show err
      pure (-1) -- Return -1 if there was an error
    Right items -> pure items

xAck :: (HedisFlow m env, TryException m) => Text -> Text -> [BS.ByteString] -> m Integer
xAck key groupName entryId = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "xAck" $ withTryCatch "xAck" (runWithPrefix'_ key $ \prefKey -> Hedis.xack prefKey (cs groupName) entryId)
    case res of
      Left err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_xAck" $ show err
      Right items -> pure items
  res <- withTimeRedis "RedisCluster" "xAck" $ withTryCatch "xAck" (runWithPrefix key $ \prefKey -> Hedis.xack prefKey (cs groupName) entryId)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_xAck" $ show err
      pure (-1) -- Return -1 if there was an error
    Right items -> pure items

lrem :: (HedisFlow m env, TryException m) => Text -> Integer -> Text -> m Integer
lrem key cnt value = withTimeRedis "RedisCluster" "lrem" $ runWithPrefix key $ \prefKey -> Hedis.lrem prefKey cnt (BSL.toStrict $ Ae.encode value)

sAddExp :: (ToJSON a, HedisFlow m env, TryException m) => Text -> [a] -> ExpirationTime -> m ()
sAddExp key members expirationTime = withLogTag "Redis" $ do
  prefKey <- buildKey key
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      standaloneRes <-
        withTryCatch "sAddExp" $ do
          void . runHedisTransaction' $ do
            void . Hedis.sadd prefKey $ map (BSL.toStrict . Ae.encode) members
            Hedis.expire prefKey (toInteger expirationTime)
      whenLeft standaloneRes (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_SADDEXP" $ show err)
    else pure ()
  clusterRes <-
    withTryCatch "sAddExp" $ do
      void . runHedisTransaction $ do
        void . Hedis.sadd prefKey $ map (BSL.toStrict . Ae.encode) members
        Hedis.expire prefKey (toInteger expirationTime)
  whenLeft clusterRes (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_SADDEXP" $ show err)

srem :: (ToJSON a, HedisFlow m env, TryException m) => Text -> [a] -> m Integer
srem key members = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "srem" $ withTryCatch "srem" (runWithPrefix'_ key $ \prefKey -> Hedis.srem prefKey $ map (BSL.toStrict . Ae.encode) members)
    case res of
      Left err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_SREM" $ show err
      Right items -> pure items
  res <- withTimeRedis "RedisCluster" "srem" $ withTryCatch "srem" (runWithPrefix key $ \prefKey -> Hedis.srem prefKey $ map (BSL.toStrict . Ae.encode) members)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_SREM" $ show err
      pure (-1) -- Returning -1 if there was an error
    Right items -> pure items

sMembers :: (FromJSON a, HedisFlow m env, TryException m) => Text -> m [a]
sMembers key = withTimeRedis "RedisCluster" "sMembers" $ do
  res <- runWithPrefix key Hedis.smembers
  mapM (\a -> Error.fromMaybeM (HedisDecodeError $ cs a) . Ae.decode $ cs a) res

ttl :: (HedisFlow m env, TryException m) => Text -> m Integer
ttl key = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "ttl" $ withTryCatch "ttl" (runWithPrefix'_ key $ \prefKey -> Hedis.ttl prefKey)
    case res of
      Left err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_TTL" $ show err
      Right expSec -> pure expSec
  res <- withTimeRedis "RedisCluster" "ttl" $ withTryCatch "ttl" (runWithPrefix key $ \prefKey -> Hedis.ttl prefKey)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_TTL" $ show err
      pure (-1) -- Returning -1 if there was an error
    Right expSec -> pure expSec

publish :: (HedisFlow m env, TryException m, ToJSON a) => Text -> a -> m ()
publish channel message = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  let encodedMessage = BSL.toStrict $ Ae.encode message
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "publish" $
      withTryCatch "publish" $
        runWithPrefix'_ channel $ \prefChannel ->
          Hedis.publish prefChannel encodedMessage
    whenLeft res $ \err ->
      withLogTag "STANDALONE" $
        logTagInfo "FAILED_TO_PUBLISH" (show err)

  res <- withTimeRedis "RedisCluster" "publish" $
    withTryCatch "publish" $
      runWithPrefix_ channel $ \prefChannel ->
        Hedis.publish prefChannel encodedMessage
  whenLeft res $ \err ->
    withLogTag "CLUSTER" $
      logTagInfo "FAILED_TO_PUBLISH" (show err)

geoAdd :: (HedisFlow m env, TryException m) => Text -> [(Double, Double, BS.ByteString)] -> m Integer
geoAdd key geoInfo = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "geoAdd" $ withTryCatch "geoAdd" (runWithPrefix'_ key $ \prefKey -> Hedis.geoadd prefKey geoInfo)
    case res of
      Left err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_GEOADD" $ show err
      Right items -> pure items
  res <- withTimeRedis "RedisCluster" "geoAdd" $ withTryCatch "geoAdd" (runWithPrefix key $ \prefKey -> Hedis.geoadd prefKey geoInfo)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_GEOADD" $ show err
      pure (-1) -- Return -1 if there was an error
    Right items -> pure items

geoSearch ::
  (HedisFlow m env, TryException m) =>
  Text ->
  -- | Search origin: either a member or coordinates.
  Hedis.GeoFrom ->
  -- | Search shape: radius or bounding box.
  Hedis.GeoBy ->
  -- | Search results.
  m [BS.ByteString]
geoSearch key from by = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      res <- withTimeRedis "RedisStandalone" "geosearch" $ withTryCatch "geoSearch" (runWithPrefix' key $ \prefKey -> Hedis.geosearch prefKey from by)
      case res of
        Left err -> do
          withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_GEOSEARCH" $ show err
          pure []
        Right items -> pure items
    else do
      res <- withTimeRedis "RedisCluster" "geosearch" $ withTryCatch "geoSearch" (runWithPrefix key $ \prefKey -> Hedis.geosearch prefKey from by)
      case res of
        Left err -> do
          withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_GEOSEARCH" $ show err
          pure [] -- Return an empty list if there was an error
        Right items -> pure items

geoSearchDecoded ::
  (FromJSON a, HedisFlow m env, TryException m) =>
  Text ->
  -- | Search origin: either a member or coordinates.
  Hedis.GeoFrom ->
  -- | Search shape: radius or bounding box.
  Hedis.GeoBy ->
  -- | Search results.
  m [a]
geoSearchDecoded key from by = withLogTag "Redis" $ do
  let decodeGeoItems :: (FromJSON a, HedisFlow m env) => [BS.ByteString] -> m [a]
      decodeGeoItems = mapM (\a -> Error.fromMaybeM (HedisDecodeError $ cs a) . Ae.decode $ cs a)
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      res <- withTimeRedis "RedisStandalone" "geosearch" $ withTryCatch "geoSearchDecoded" (runWithPrefix' key $ \prefKey -> Hedis.geosearch prefKey from by)
      case res of
        Left err -> do
          withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_GEOSEARCH" $ show err
          pure []
        Right items -> decodeGeoItems items
    else do
      res <- withTimeRedis "RedisCluster" "geosearch" $ withTryCatch "geoSearchDecoded" (runWithPrefix key $ \prefKey -> Hedis.geosearch prefKey from by)
      case res of
        Left err -> do
          withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_GEOSEARCH" $ show err
          pure [] -- Return an empty list if there was an error
        Right items -> decodeGeoItems items

setTtlIfNone :: (HedisFlow m env, TryException m) => Text -> ExpirationTime -> m ()
setTtlIfNone key secs = do
  current <- ttl key
  when (current < 0) $
    expire key secs
