{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.Hedis.Queries where

import qualified Data.Aeson as Ae
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.String.Conversions
import Data.Text hiding (concatMap, map, null)
import qualified Data.Text as T
import qualified Data.Text as Text
import Database.Redis (Queued, Redis, RedisTx, Reply, TxResult (..))
import qualified Database.Redis as Hedis
import EulerHS.Prelude (whenLeft)
import GHC.Records.Extra
import Kernel.Prelude
import Kernel.Storage.Hedis.Config
import Kernel.Storage.Hedis.Error
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

convertFromHedisRecord :: Hedis.StreamsRecord -> StreamsRecord
convertFromHedisRecord hedisRecord =
  StreamsRecord
    { recordId = Hedis.recordId hedisRecord,
      keyValues = Hedis.keyValues hedisRecord
    }

runHedis ::
  HedisFlow m env => Redis (Either Reply a) -> m a
runHedis action = do
  eithRes <- runHedisEither action
  Error.fromEitherM (HedisReplyError . show) eithRes

runHedisEither ::
  HedisFlow m env => Redis (Either Reply a) -> m (Either Reply a)
runHedisEither action = do
  con <- asks (.hedisClusterEnv.hedisConnection)
  liftIO $ Hedis.runRedis con action

runHedisTransaction ::
  HedisFlow m env => RedisTx (Queued a) -> m a
runHedisTransaction action = do
  con <- asks (.hedisClusterEnv.hedisConnection)
  res <- liftIO . Hedis.runRedis con $ Hedis.multiExec action
  case res of
    TxError err -> Error.throwError $ HedisReplyError err
    TxAborted -> Error.throwError HedisTransactionAborted
    TxSuccess a -> return a

----------------------------------------------------

-- Just remove key modifier, so it won't modify the key with app prefixes
withCrossAppRedis ::
  (HedisFlow m env) => m f -> m f
withCrossAppRedis f = do
  local (\env -> env{hedisEnv = env.hedisEnv{keyModifier = identity}, hedisClusterEnv = env.hedisClusterEnv{keyModifier = identity}}) f

withNonCriticalCrossAppRedis ::
  (HedisFlow m env) => m f -> m f
withNonCriticalCrossAppRedis f = do
  local (\env -> env{hedisEnv = env.hedisNonCriticalEnv{keyModifier = identity}, hedisClusterEnv = env.hedisNonCriticalClusterEnv{keyModifier = identity}}) f

withNonCriticalRedis ::
  (HedisFlow m env) => m f -> m f
withNonCriticalRedis f = do
  local (\env -> env{hedisEnv = env.hedisNonCriticalEnv, hedisClusterEnv = env.hedisNonCriticalClusterEnv}) f

buildKey :: HedisFlow m env => Text -> m BS.ByteString
buildKey key = do
  keyModifier <- asks (.hedisEnv.keyModifier)
  return . cs $ keyModifier key

runWithPrefixEither :: (HedisFlow m env) => Text -> (BS.ByteString -> Redis (Either Reply a)) -> m (Either Reply a)
runWithPrefixEither key action = do
  prefKey <- buildKey key
  withLogTag "Redis" $ logDebug $ "working with key : " <> cs prefKey
  runHedisEither $ action prefKey

runWithPrefix :: (HedisFlow m env) => Text -> (BS.ByteString -> Redis (Either Reply a)) -> m a
runWithPrefix key action = do
  prefKey <- buildKey key
  withLogTag "Redis" $ logDebug $ "working with key : " <> cs prefKey
  runHedis $ action prefKey

runWithPrefix_ :: (HedisFlow m env) => Text -> (BS.ByteString -> Redis (Either Reply a)) -> m ()
runWithPrefix_ key action = void $ runWithPrefix key action

runWithPrefix'_ :: (HedisFlow m env) => Text -> (BS.ByteString -> Redis (Either Reply a)) -> m ()
runWithPrefix'_ key action = void $ runWithPrefix' key action

runWithPrefix' :: (HedisFlow m env) => Text -> (BS.ByteString -> Redis (Either Reply a)) -> m a
runWithPrefix' key action = do
  prefKey <- buildKey key
  withLogTag "Redis" $ logDebug $ "working with key : " <> cs prefKey
  runHedis' $ action prefKey

runHedisTransaction' ::
  HedisFlow m env => RedisTx (Queued a) -> m a
runHedisTransaction' action = do
  con <- asks (.hedisEnv.hedisConnection)
  res <- liftIO . Hedis.runRedis con $ Hedis.multiExec action
  case res of
    TxError err -> Error.throwError $ HedisReplyError err
    TxAborted -> Error.throwError HedisTransactionAborted
    TxSuccess a -> return a

runHedis' ::
  HedisFlow m env => Redis (Either Reply a) -> m a
runHedis' action = do
  eithRes <- runHedisEither' action
  Error.fromEitherM (HedisReplyError . show) eithRes

runHedisEither' ::
  HedisFlow m env => Redis (Either Reply a) -> m (Either Reply a)
runHedisEither' action = do
  con <- asks (.hedisEnv.hedisConnection)
  liftIO $ Hedis.runRedis con action

tryGetFromStandalone :: HedisFlow m env => Text -> m (Maybe BS.ByteString)
tryGetFromStandalone key = withLogTag "STANDALONE" $ do
  eitherMaybeBS <- withTimeRedis "RedisStandalone" "get" $ try @_ @SomeException (runWithPrefix' key Hedis.get)
  case eitherMaybeBS of
    Left err -> logTagInfo "ERROR_WHILE_GET" (show err) $> Nothing
    Right maybeBS -> pure maybeBS

tryGetFromCluster :: HedisFlow m env => Text -> m (Maybe BS.ByteString)
tryGetFromCluster key = withLogTag "CLUSTER" $ do
  eitherMaybeBS <- withTimeRedis "RedisCluster" "get" $ try @_ @SomeException (runWithPrefix key Hedis.get)
  case eitherMaybeBS of
    Left err -> logTagInfo "ERROR_WHILE_GET" (show err) $> Nothing
    Right maybeBS -> pure maybeBS

getImpl :: (FromJSON a, HedisFlow m env) => (BS.ByteString -> m (Maybe a)) -> Text -> m (Maybe a)
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

get :: (FromJSON a, HedisFlow m env) => Text -> m (Maybe a)
get = getImpl decodeResult
  where
    decodeResult bs = Error.fromMaybeM (HedisDecodeError $ cs bs) $ Ae.decode $ BSL.fromStrict bs

get' ::
  (FromJSON a, HedisFlow m env) => Text -> m () -> m (Maybe a)
get' key decodeErrHandler = getImpl decodeResult key
  where
    decodeResult bs =
      case Ae.decode $ BSL.fromStrict bs of
        Just a -> return $ Just a
        Nothing -> do
          logTagError "REDIS" $ "Decode Failure for key:" <> key <> ", with value:" <> cs bs
          decodeErrHandler
          return Nothing

safeGet ::
  (FromJSON a, HedisFlow m env) => Text -> m (Maybe a)
safeGet key = get' key (del key)

set ::
  (ToJSON a, HedisFlow m env) => Text -> a -> m ()
set key val = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      res <- withTimeRedis "RedisStandalone" "set" $ try @_ @SomeException (runWithPrefix'_ key $ \prefKey -> Hedis.set prefKey $ BSL.toStrict $ Ae.encode val)
      whenLeft res (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_SET" $ show err)
    else pure ()
  res <- withTimeRedis "RedisCluster" "set" $ try @_ @SomeException (runWithPrefix_ key $ \prefKey -> Hedis.set prefKey $ BSL.toStrict $ Ae.encode val)
  whenLeft res (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_SET" $ show err)

setExp ::
  (ToJSON a, HedisFlow m env) => Text -> a -> ExpirationTime -> m ()
setExp key val expirationTime = withTimeRedis "Redis" "setExp" . withLogTag "Redis" $ do
  prefKey <- buildKey key
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      standaloneRes <-
        withTimeRedis "RedisStandalone" "setExp" $
          try @_ @SomeException $ do
            void . runHedisTransaction' $ do
              void . Hedis.set prefKey $ BSL.toStrict $ Ae.encode val
              Hedis.expire prefKey (toInteger expirationTime)
      whenLeft standaloneRes (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_SETEXP" $ show err)
    else pure ()
  clusterRes <-
    withTimeRedis "RedisCluster" "setExp" $
      try @_ @SomeException $ do
        void . runHedisTransaction $ do
          void . Hedis.set prefKey $ BSL.toStrict $ Ae.encode val
          Hedis.expire prefKey (toInteger expirationTime)
  whenLeft clusterRes (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_SETEXP" $ show err)

setNx ::
  (ToJSON a, HedisFlow m env) => Text -> a -> m Bool
setNx key val = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  writtenInStandalone <-
    if migrating
      then do
        standaloneRes <- withTimeRedis "RedisStandalone" "setNx" $ try @_ @SomeException $ runWithPrefix' key $ \prefKey -> Hedis.setnx prefKey $ BSL.toStrict $ Ae.encode val
        case standaloneRes of
          Left err -> withLogTag "STANDALONE" (logTagInfo "FAILED_TO_SETNX" $ show err) $> False
          Right res -> pure res
      else pure False
  clusterRes <- withTimeRedis "RedisCluster" "setNx" $ try @_ @SomeException $ runWithPrefix key $ \prefKey -> Hedis.setnx prefKey $ BSL.toStrict $ Ae.encode val
  case clusterRes of
    Left err -> withLogTag "CLUSTER" (logTagInfo "FAILED_TO_SETNX" $ show err) $> (writtenInStandalone || False)
    Right res -> pure $ writtenInStandalone || res

del :: (HedisFlow m env) => Text -> m ()
del key = withLogTag "Redis" do
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      res <- withTimeRedis "RedisStandalone" "del" $ try @_ @SomeException (runWithPrefix'_ key $ \prefKey -> Hedis.del [prefKey])
      whenLeft res (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_DELETE" $ show err)
    else pure ()
  res <- withTimeRedis "RedisCluster" "del" $ try @_ @SomeException (runWithPrefix_ key $ \prefKey -> Hedis.del [prefKey])
  whenLeft res (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_DELETE" $ show err)

rPushExp :: (HedisFlow m env, ToJSON a) => Text -> [a] -> ExpirationTime -> m ()
rPushExp key list ex = withLogTag "Redis" $ do
  prefKey <- buildKey key
  logDebug $ "working with key : " <> cs prefKey
  unless (null list) $ do
    migrating <- asks (.hedisMigrationStage)
    if migrating
      then do
        standaloneRes <-
          withTimeRedis "RedisStandalone" "rPushExp" $ do
            try @_ @SomeException $ do
              void . runHedisTransaction' $ do
                void . Hedis.rpush prefKey $ map (BSL.toStrict . Ae.encode) list
                Hedis.expire prefKey (toInteger ex)
        whenLeft standaloneRes (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_PUSHEXP" $ show err)
      else pure ()
    clusterRes <-
      withTimeRedis "RedisCluster" "rPushExp" $ do
        try @_ @SomeException $ do
          void . runHedisTransaction $ do
            void . Hedis.rpush prefKey $ map (BSL.toStrict . Ae.encode) list
            Hedis.expire prefKey (toInteger ex)
    whenLeft clusterRes (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_PUSHEXP" $ show err)

lPush :: (HedisFlow m env, ToJSON a) => Text -> NonEmpty a -> m ()
lPush key list = withTimeRedis "RedisCluster" "lPush" . runWithPrefix_ key $ \prefKey ->
  Hedis.lpush prefKey $ map (BSL.toStrict . Ae.encode) (toList list)

rPush :: (HedisFlow m env, ToJSON a) => Text -> NonEmpty a -> m ()
rPush key list = withTimeRedis "RedisCluster" "rPush" . runWithPrefix_ key $ \prefKey ->
  Hedis.rpush prefKey $ map (BSL.toStrict . Ae.encode) (toList list)

rPop :: (HedisFlow m env, FromJSON a) => Text -> m (Maybe a)
rPop key = withTimeRedis "RedisCluster" "rPop" $ do
  res <- runWithPrefix key $ \prefKey -> Hedis.rpop prefKey
  pure $ Ae.decode . BSL.fromStrict =<< res

lTrim :: (HedisFlow m env) => Text -> Integer -> Integer -> m ()
lTrim key start stop = withTimeRedis "RedisCluster" "lTrim" . runWithPrefix_ key $ \prefKey ->
  Hedis.ltrim prefKey start stop

clearList :: (HedisFlow m env) => Text -> m ()
clearList key = lTrim key 2 1

lLen :: (HedisFlow m env) => Text -> m Integer
lLen key = withTimeRedis "RedisCluster" "lLen" $ runWithPrefix key Hedis.llen

lRange :: (HedisFlow m env, FromJSON a) => Text -> Integer -> Integer -> m [a]
lRange key start stop = withTimeRedis "RedisCluster" "lRange" $ do
  res <- runWithPrefix key $ \prefKey ->
    Hedis.lrange prefKey start stop
  mapM (\a -> Error.fromMaybeM (HedisDecodeError $ cs a) . Ae.decode $ cs a) res

getList :: (HedisFlow m env, FromJSON a) => Text -> m [a]
getList key = lRange key 0 (-1)

incr :: (HedisFlow m env) => Text -> m Integer
incr key = withTimeRedis "RedisCluster" "incr" $ runWithPrefix key Hedis.incr

incrby :: (HedisFlow m env) => Text -> Integer -> m Integer
incrby key val = withTimeRedis "RedisCluster" "incrBy" $ runWithPrefix key $ flip Hedis.incrby val

decr :: (HedisFlow m env) => Text -> m Integer
decr key = withTimeRedis "RedisCluster" "decr" $ runWithPrefix key Hedis.decr

decrby :: (HedisFlow m env) => Text -> Integer -> m Integer
decrby key val = withTimeRedis "RedisCluster" "decrBy" $ runWithPrefix key $ flip Hedis.decrby val

incrByFloat :: (HedisFlow m env) => Text -> Double -> m Double
incrByFloat key toAdd = withTimeRedis "RedisCluster" "incrByFloat" . runWithPrefix key $ \prefKey ->
  Hedis.incrbyfloat prefKey toAdd

expire :: (HedisFlow m env) => Text -> ExpirationTime -> m ()
expire key expirationTime = do
  migrating <- asks (.hedisMigrationStage)
  when migrating . withTimeRedis "RedisStandalone" "expire" . runWithPrefix'_ key $ \prefKey -> Hedis.expire prefKey (toInteger expirationTime)
  withTimeRedis "RedisCluster" "expire" . runWithPrefix_ key $ \prefKey -> Hedis.expire prefKey (toInteger expirationTime)

setNxExpire :: (ToJSON a, HedisFlow m env) => Text -> ExpirationTime -> a -> m Bool
setNxExpire key expirationTime val = withTimeRedis "RedisCluster" "setNxExpire" $ do
  eithRes <- runWithPrefixEither key $ \prefKey ->
    Hedis.setOpts prefKey (cs $ Ae.encode val) $
      Hedis.SetOpts (Just $ toInteger expirationTime) Nothing (Just Hedis.Nx)
  pure $ case eithRes of
    Right Hedis.Ok -> True
    _ -> False

delByPattern :: HedisFlow m env => Text -> m ()
delByPattern ptrn = withTimeRedis "RedisCluster" "delByPattern" $ do
  runWithPrefix_ ptrn $ \prefKey ->
    Hedis.eval @_ @_ @Reply "for i, name in ipairs(redis.call('KEYS', ARGV[1])) do redis.call('DEL', name); end" ["0"] [prefKey]

tryLockRedis :: HedisFlow m env => Text -> ExpirationTime -> m Bool
tryLockRedis key timeout = setNxExpire (buildLockResourceName key) timeout ()

unlockRedis :: HedisFlow m env => Text -> m ()
unlockRedis key = void . del $ buildLockResourceName key

whenWithLockRedis :: (HedisFlow m env, MonadMask m) => Text -> ExpirationTime -> m () -> m ()
whenWithLockRedis key timeout func = do
  whenM (tryLockRedis key timeout) $ do
    finally func $ unlockRedis key

withLockRedis :: (HedisFlow m env, MonadMask m) => Text -> ExpirationTime -> m () -> m ()
withLockRedis key timeout func = do
  getLock
  finally func (unlockRedis key)
  where
    getLock = do
      lockAvailable <- tryLockRedis key timeout
      unless lockAvailable getLock

withLockRedisAndReturnValue :: (HedisFlow m env, MonadMask m) => Text -> ExpirationTime -> m a -> m a
withLockRedisAndReturnValue key timeout func = do
  getLock
  finally func (unlockRedis key)
  where
    getLock = do
      lockAvailable <- tryLockRedis key timeout
      unless lockAvailable getLock

withWaitOnLockRedisWithExpiry :: (HedisFlow m env, MonadMask m) => Text -> ExpirationTime -> ExpirationTime -> m () -> m ()
withWaitOnLockRedisWithExpiry key timeout recursionTimeOut func = do
  uuid <- T.pack <$> liftIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)
  let keyE = "recursion timeout for:" <> uuid
  setExp keyE True recursionTimeOut
  withWaitOnLockRedisWithExpiry' keyE key timeout func

withWaitOnLockRedisWithExpiry' :: (HedisFlow m env, MonadMask m) => Text -> Text -> ExpirationTime -> m () -> m ()
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

hSetExp :: (ToJSON a, HedisFlow m env) => Text -> Text -> a -> ExpirationTime -> m ()
hSetExp key field value expirationTime = withLogTag "Redis" $ do
  prefKey <- buildKey key
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      standaloneRes <-
        withTimeRedis "RedisStandalone" "hSetExp" $ do
          try @_ @SomeException $ do
            void . runHedisTransaction' $ do
              void . Hedis.hset prefKey (cs field) $ BSL.toStrict $ Ae.encode value
              Hedis.expire prefKey (toInteger expirationTime)
      whenLeft standaloneRes (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_HSETEXP" $ show err)
    else pure ()
  clusterRes <-
    withTimeRedis "RedisCluster" "hSetExp" $ do
      try @_ @SomeException $ do
        void . runHedisTransaction $ do
          void . Hedis.hset prefKey (cs field) $ BSL.toStrict $ Ae.encode value
          Hedis.expire prefKey (toInteger expirationTime)
  whenLeft clusterRes (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_HSETEXP" $ show err)

hGet :: (FromJSON a, HedisFlow m env) => Text -> Text -> m (Maybe a)
hGet key field =
  withTimeRedis "RedisCluster" "hGet" $ do
    maybeBS <- runWithPrefix key (`Hedis.hget` cs field)
    case maybeBS of
      Nothing -> pure Nothing
      Just bs -> Error.fromMaybeM (HedisDecodeError $ cs bs) $ Ae.decode $ BSL.fromStrict bs

hDel :: HedisFlow m env => Text -> [Text] -> m ()
hDel key fields = withTimeRedis "RedisCluster" "hDel" $ runWithPrefix_ key (`Hedis.hdel` map cs fields)

hGetAll :: (FromJSON a, HedisFlow m env) => Text -> m [(Text, a)]
hGetAll key = withTimeRedis "RedisCluster" "hGetAll" $ do
  hMap <- runWithPrefix key Hedis.hgetall
  pure $ mapMaybe (\(k, val) -> (cs k,) <$> Ae.decode (BSL.fromStrict val)) hMap

zAddExp :: (ToJSON Integer, HedisFlow m env) => Text -> Text -> Integer -> ExpirationTime -> m ()
zAddExp key field value expirationTime = withLogTag "Redis" $ do
  prefKey <- buildKey key
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      standaloneRes <-
        try @_ @SomeException $ do
          void . runHedisTransaction' $ do
            void . Hedis.zadd prefKey $ [(fromIntegral value, cs field)]
            Hedis.expire prefKey (toInteger expirationTime)
      whenLeft standaloneRes (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_ZADDEXP" $ show err)
    else pure ()
  clusterRes <-
    try @_ @SomeException $ do
      void . runHedisTransaction $ do
        void . Hedis.zadd prefKey $ [(fromIntegral value, cs field)]
        Hedis.expire prefKey (toInteger expirationTime)
  whenLeft clusterRes (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZADDEXP" $ show err)

zrevrangeWithscores :: (HedisFlow m env) => Text -> Integer -> Integer -> m [(Text, Double)]
zrevrangeWithscores key start stop = do
  res <- runWithPrefix key $ \prefKey ->
    Hedis.zrevrangeWithscores prefKey start stop
  pure $ map (\(k, score) -> (cs' k, score)) res
  where
    cs' :: BS.ByteString -> Text
    cs' = cs

zScore :: (FromJSON Double, HedisFlow m env) => Text -> Text -> m (Maybe Double)
zScore key member = do
  maybeZS <- runWithPrefix key (`Hedis.zscore` cs member)
  pure maybeZS

zRevRank :: (FromJSON Integer, HedisFlow m env) => Text -> Text -> m (Maybe Integer)
zRevRank key member = do
  maybeZr <- runWithPrefix key (`Hedis.zrevrank` cs member)
  pure maybeZr

zCard :: (HedisFlow m env) => Text -> m Integer
zCard key = runWithPrefix key Hedis.zcard

zAdd ::
  (ToJSON member, HedisFlow m env) =>
  Text ->
  [(Double, member)] ->
  m ()
zAdd key members = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      res <- withTimeRedis "RedisStandalone" "zAdd" $ try @_ @SomeException (runWithPrefix'_ key $ \prefKey -> Hedis.zadd prefKey $ map (\(score, member) -> (score, BSL.toStrict $ Ae.encode member)) members)
      whenLeft res (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_ZADD" $ show err)
    else pure ()
  res <- withTimeRedis "RedisCluster" "zAdd" $ try @_ @SomeException (runWithPrefix_ key $ \prefKey -> Hedis.zadd prefKey $ map (\(score, member) -> (score, BSL.toStrict $ Ae.encode member)) members)
  whenLeft res (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZADD" $ show err)

xInfoGroups ::
  (HedisFlow m env) =>
  Text -> -- Stream key
  m Bool
xInfoGroups key = do
  migrating <- asks (.hedisMigrationStage)
  eitherMaybeBS <-
    if migrating
      then withTimeRedis "RedisStandalone" "xInfoGroups" $ try @_ @SomeException (runWithPrefix' key Hedis.xinfoGroups)
      else withTimeRedis "RedisCluster" "xInfoGroups" $ try @_ @SomeException (runWithPrefix key Hedis.xinfoGroups)
  ls <-
    case eitherMaybeBS of
      Left err -> logTagInfo "ERROR_WHILE_GET_XInfoGroups" (show err) $> []
      Right maybeBS -> pure maybeBS
  return $ not (null ls)

-- Function to create a new consumer group for a stream
xGroupCreate ::
  (HedisFlow m env) =>
  Text -> -- Stream key
  Text -> -- Group name
  Text -> -- Start ID
  m ()
xGroupCreate key groupName startId = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "xGroupCreate" $ try @_ @SomeException (runWithPrefix'_ key $ \prefKey -> Hedis.xgroupCreate prefKey (cs groupName) (cs startId))
    whenLeft res (withLogTag "STANDALONE" . logTagInfo "FAILED_TO_xGroupCreate" . show)
  res <- withTimeRedis "RedisCluster" "xGroupCreate" $ try @_ @SomeException (runWithPrefix_ key $ \prefKey -> Hedis.xgroupCreate prefKey (cs groupName) (cs startId))
  whenLeft res (withLogTag "CLUSTER" . logTagInfo "FAILED_TO_xGroupCreate" . show)

extractKeyValuePairs :: [StreamsRecord] -> [(Text, Text)]
extractKeyValuePairs = concatMap (\(StreamsRecord _ keyVals) -> map (\(k, v) -> (cs k, cs v)) keyVals)

extractRecordIds :: [StreamsRecord] -> [BS.ByteString]
extractRecordIds = map (\(StreamsRecord recordId _) -> recordId)

xReadGroup ::
  (HedisFlow m env) =>
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
          then withTimeRedis "RedisStandalone" "XReadGroup" $ try @_ @SomeException (runWithPrefix' (cs $ fst keyVal) $ \_ -> Hedis.xreadGroup (cs groupName) (cs consumerName) bsPairsList)
          else withTimeRedis "RedisCluster" "XReadGroup" $ try @_ @SomeException (runWithPrefix (cs $ fst keyVal) $ \_ -> Hedis.xreadGroup (cs groupName) (cs consumerName) bsPairsList)
      mbRes <-
        case eitherMaybeBS of
          Left err -> logTagInfo "ERROR_WHILE_GET_XReadGroup" (show err) $> Nothing
          Right maybeBS -> pure maybeBS
      case mbRes of
        Just res -> return $ Just (map convertFromHedisResponse res)
        Nothing -> pure Nothing
    Nothing -> pure Nothing

xReadGroupOpts ::
  (HedisFlow m env) =>
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
          then withTimeRedis "RedisStandalone" "xReadGroupOpts" $ try @_ @SomeException (runWithPrefix' (cs $ fst keyVal) $ \_ -> Hedis.xreadGroupOpts (cs groupName) (cs consumerName) bsPairsList opts)
          else withTimeRedis "RedisCluster" "xReadGroupOpts" $ try @_ @SomeException (runWithPrefix (cs $ fst keyVal) $ \_ -> Hedis.xreadGroupOpts (cs groupName) (cs consumerName) bsPairsList opts)
      mbRes <-
        case eitherMaybeBS of
          Left err -> logTagInfo "ERROR_WHILE_GET_XReadGroupOpts" (show err) $> Nothing
          Right maybeBS -> pure maybeBS
      case mbRes of
        Just res -> return $ Just (map convertFromHedisResponse res)
        Nothing -> pure Nothing
    Nothing -> pure Nothing

xAdd :: (HedisFlow m env) => Text -> Text -> [(BS.ByteString, BS.ByteString)] -> m BS.ByteString
xAdd key entryId fieldValues = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "xadd" $ try @_ @SomeException (runWithPrefix'_ key $ \prefKey -> Hedis.xadd prefKey (cs entryId) fieldValues)
    whenLeft res (withLogTag "STANDALONE" . logTagInfo "FAILED_TO_xadd" . show)
  res <- withTimeRedis "RedisCluster" "xadd" $ try @_ @SomeException (runWithPrefix key $ \prefKey -> Hedis.xadd prefKey (cs entryId) fieldValues)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "xadd" $ show err
      pure ""
    Right items -> pure items

xAddExp :: (HedisFlow m env) => Text -> Text -> [(BS.ByteString, BS.ByteString)] -> ExpirationTime -> m ()
xAddExp key entryId fieldValues expirationTime = withLogTag "Redis" $ do
  prefKey <- buildKey key
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <-
      withTimeRedis "RedisStandalone" "xaddExp" $
        try @_ @SomeException $ do
          void $
            runHedisTransaction' $ do
              void $ Hedis.xadd prefKey (cs entryId) fieldValues
              Hedis.expire prefKey (toInteger expirationTime)
    whenLeft res (withLogTag "STANDALONE" . logTagInfo "FAILED_TO_xaddExp" . show)
  clusterRes <-
    withTimeRedis "RedisCluster" "xaddExp" $
      try @_ @SomeException $ do
        void $
          runHedisTransaction $ do
            void $ Hedis.xadd prefKey (cs entryId) fieldValues
            Hedis.expire prefKey (toInteger expirationTime)
  whenLeft clusterRes (withLogTag "CLUSTER" . logTagInfo "FAILED_TO_XADDEXP" . show)

zRangeByScoreByCount :: (HedisFlow m env) => Text -> Double -> Double -> Integer -> Integer -> m [BS.ByteString]
zRangeByScoreByCount key start end offset limit = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "zRangeByScoreByCount" $ try @_ @SomeException (runWithPrefix'_ key $ \prefKey -> Hedis.zrangebyscoreLimit prefKey start end offset limit)
    whenLeft res (withLogTag "STANDALONE" . logTagInfo "FAILED_TO_ZRANGEBYSCOREBYCOUNT" . show)
  res <- withTimeRedis "RedisCluster" "zRangeByScoreByCount" $ try @_ @SomeException (runWithPrefix key $ \prefKey -> Hedis.zrangebyscoreLimit prefKey start end offset limit)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZRANGEBYSCOREBYCOUNT" $ show err
      pure [] -- Return an empty list if there was an error
    Right items -> pure items

zRangeByScore :: (HedisFlow m env) => Text -> Double -> Double -> m [BS.ByteString]
zRangeByScore key start end = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "zRangeByScore" $ try @_ @SomeException (runWithPrefix'_ key $ \prefKey -> Hedis.zrangebyscore prefKey start end)
    whenLeft res (withLogTag "STANDALONE" . logTagInfo "FAILED_TO_ZRANGEBYSCORE" . show)
  res <- withTimeRedis "RedisCluster" "zRangeByScore" $ try @_ @SomeException (runWithPrefix key $ \prefKey -> Hedis.zrangebyscore prefKey start end)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZRANGEBYSCORE" $ show err
      pure [] -- Return an empty list if there was an error
    Right items -> pure items

zRemRangeByScore :: (HedisFlow m env) => Text -> Double -> Double -> m Integer
zRemRangeByScore key start end = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "zRemRangeByScore" $ try @_ @SomeException (runWithPrefix'_ key $ \prefKey -> Hedis.zremrangebyscore prefKey start end)
    case res of
      Left err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_ZREMRANGEBYSCORE" $ show err
      Right items -> pure items
  res <- withTimeRedis "RedisCluster" "zRemRangeByScore" $ try @_ @SomeException (runWithPrefix key $ \prefKey -> Hedis.zremrangebyscore prefKey start end)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_ZREMRANGEBYSCORE" $ show err
      pure (-1) -- Return -1 if there was an error
    Right items -> pure items

xDel :: (HedisFlow m env) => Text -> [BS.ByteString] -> m Integer
xDel key entryId = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "xDel" $ try @_ @SomeException (runWithPrefix'_ key $ \prefKey -> Hedis.xdel prefKey entryId)
    case res of
      Left err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_XDEL" $ show err
      Right items -> pure items
  res <- withTimeRedis "RedisCluster" "xDel" $ try @_ @SomeException (runWithPrefix key $ \prefKey -> Hedis.xdel prefKey entryId)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_XDEL" $ show err
      pure (-1) -- Return -1 if there was an error
    Right items -> pure items

xAck :: (HedisFlow m env) => Text -> Text -> [BS.ByteString] -> m Integer
xAck key groupName entryId = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  when migrating $ do
    res <- withTimeRedis "RedisStandalone" "xAck" $ try @_ @SomeException (runWithPrefix'_ key $ \prefKey -> Hedis.xack prefKey (cs groupName) entryId)
    case res of
      Left err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_xAck" $ show err
      Right items -> pure items
  res <- withTimeRedis "RedisCluster" "xAck" $ try @_ @SomeException (runWithPrefix key $ \prefKey -> Hedis.xack prefKey (cs groupName) entryId)
  case res of
    Left err -> do
      withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_xAck" $ show err
      pure (-1) -- Return -1 if there was an error
    Right items -> pure items

lrem :: (HedisFlow m env) => Text -> Integer -> Text -> m Integer
lrem key cnt value = withTimeRedis "RedisCluster" "lrem" $ runWithPrefix key $ \prefKey -> Hedis.lrem prefKey cnt (BSL.toStrict $ Ae.encode value)
