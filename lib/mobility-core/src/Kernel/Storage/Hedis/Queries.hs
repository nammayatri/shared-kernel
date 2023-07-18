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
import Data.Text hiding (map, null)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as DE
import Database.Redis (Queued, Redis, RedisTx, Reply, TxResult (..))
import qualified Database.Redis as Hedis
import EulerHS.Prelude (whenLeft)
import GHC.Records.Extra
import Kernel.Prelude
import Kernel.Storage.Hedis.Config
import Kernel.Storage.Hedis.Error
import Kernel.Types.MonadGuid
import Kernel.Utils.DatastoreLatencyCalculator
import qualified Kernel.Utils.Error.Throwing as Error
import Kernel.Utils.Logging

type ExpirationTime = Int

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

getValue :: (FromJSON a, HedisFlow m env) => Text -> m (Maybe a)
getValue key = getImpl decodeResult key
  where
    decodeResult bs = Error.fromMaybeM (HedisDecodeError $ cs bs) $ Ae.decode $ BSL.fromStrict bs

getValue' ::
  (FromJSON a, HedisFlow m env) => Text -> m () -> m (Maybe a)
getValue' key decodeErrHandler = getImpl decodeResult key
  where
    decodeResult bs =
      case Ae.decode $ BSL.fromStrict bs of
        Just a -> return $ Just a
        Nothing -> do
          decodeErrHandler
          return Nothing

safeGetValue ::
  (FromJSON a, HedisFlow m env) => Text -> m (Maybe a)
safeGetValue key = getValue' key (delValue key)

setValue ::
  (ToJSON a, HedisFlow m env) => Text -> a -> m ()
setValue key val = withLogTag "Redis" $ do
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      res <- withTimeRedis "RedisStandalone" "set" $ try @_ @SomeException (runWithPrefix'_ key $ \prefKey -> Hedis.set prefKey $ BSL.toStrict $ Ae.encode val)
      whenLeft res (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_SET" $ show err)
    else pure ()
  res <- withTimeRedis "RedisCluster" "set" $ try @_ @SomeException (runWithPrefix_ key $ \prefKey -> Hedis.set prefKey $ BSL.toStrict $ Ae.encode val)
  whenLeft res (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_SET" $ show err)

setExpValue ::
  (ToJSON a, HedisFlow m env) => Text -> a -> ExpirationTime -> m ()
setExpValue key val expirationTime = withTimeRedis "Redis" "setExp" . withLogTag "Redis" $ do
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

setNxValue ::
  (ToJSON a, HedisFlow m env) => Text -> a -> m Bool
setNxValue key val = withLogTag "Redis" $ do
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

delValue :: (HedisFlow m env) => Text -> m ()
delValue key = withLogTag "Redis" do
  migrating <- asks (.hedisMigrationStage)
  if migrating
    then do
      res <- withTimeRedis "RedisStandalone" "del" $ try @_ @SomeException (runWithPrefix'_ key $ \prefKey -> Hedis.del [prefKey])
      whenLeft res (\err -> withLogTag "STANDALONE" $ logTagInfo "FAILED_TO_DELETE" $ show err)
    else pure ()
  res <- withTimeRedis "RedisCluster" "del" $ try @_ @SomeException (runWithPrefix_ key $ \prefKey -> Hedis.del [prefKey])
  whenLeft res (\err -> withLogTag "CLUSTER" $ logTagInfo "FAILED_TO_DELETE" $ show err)

rPushExpValue :: (HedisFlow m env, ToJSON a) => Text -> [a] -> ExpirationTime -> m ()
rPushExpValue key list ex = withLogTag "Redis" $ do
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

lPushValue :: (HedisFlow m env, ToJSON a) => Text -> NonEmpty a -> m ()
lPushValue key list = withTimeRedis "RedisCluster" "lPush" . runWithPrefix_ key $ \prefKey ->
  Hedis.lpush prefKey $ map (BSL.toStrict . Ae.encode) (toList list)

rPushValue :: (HedisFlow m env, ToJSON a) => Text -> NonEmpty a -> m ()
rPushValue key list = withTimeRedis "RedisCluster" "rPush" . runWithPrefix_ key $ \prefKey ->
  Hedis.rpush prefKey $ map (BSL.toStrict . Ae.encode) (toList list)

rPopValue :: (HedisFlow m env, FromJSON a) => Text -> m (Maybe a)
rPopValue key = withTimeRedis "RedisCluster" "rPop" $ do
  res <- runWithPrefix key $ \prefKey -> Hedis.rpop prefKey
  pure $ Ae.decode . BSL.fromStrict =<< res

lTrimValue :: (HedisFlow m env) => Text -> Integer -> Integer -> m ()
lTrimValue key start stop = withTimeRedis "RedisCluster" "lTrim" . runWithPrefix_ key $ \prefKey ->
  Hedis.ltrim prefKey start stop

--clearList :: (HedisFlow m env) => Text -> m ()
--clearList key = lTrimValue key 2 1

lLenValue :: (HedisFlow m env) => Text -> m Integer
lLenValue key = withTimeRedis "RedisCluster" "lLen" $ runWithPrefix key Hedis.llen

lRangeValue :: (HedisFlow m env, FromJSON a) => Text -> Integer -> Integer -> m [a]
lRangeValue key start stop = withTimeRedis "RedisCluster" "lRange" $ do
  res <- runWithPrefix key $ \prefKey ->
    Hedis.lrange prefKey start stop
  mapM (\a -> Error.fromMaybeM (HedisDecodeError $ cs a) . Ae.decode $ cs a) res

--getList :: (HedisFlow m env, FromJSON a) => Text -> m [a]
--getList key = lRangeValue key 0 (-1)

incrValue :: (HedisFlow m env) => Text -> m Integer
incrValue key = withTimeRedis "RedisCluster" "incr" $ runWithPrefix key Hedis.incr

incrValueby :: (HedisFlow m env) => Text -> Integer -> m Integer
incrValueby key val = withTimeRedis "RedisCluster" "incrBy" $ runWithPrefix key $ flip Hedis.incrby val

decrValue :: (HedisFlow m env) => Text -> m Integer
decrValue key = withTimeRedis "RedisCluster" "decr" $ runWithPrefix key Hedis.decr

decrValueby :: (HedisFlow m env) => Text -> Integer -> m Integer
decrValueby key val = withTimeRedis "RedisCluster" "decrBy" $ runWithPrefix key $ flip Hedis.decrby val

incrValueByFloat :: (HedisFlow m env) => Text -> Double -> m Double
incrValueByFloat key toAdd = withTimeRedis "RedisCluster" "incrByFloat" . runWithPrefix key $ \prefKey ->
  Hedis.incrbyfloat prefKey toAdd

expireValue :: (HedisFlow m env) => Text -> ExpirationTime -> m ()
expireValue key expirationTime = do
  migrating <- asks (.hedisMigrationStage)
  when migrating . withTimeRedis "RedisStandalone" "expire" . runWithPrefix'_ key $ \prefKey -> Hedis.expire prefKey (toInteger expirationTime)
  withTimeRedis "RedisCluster" "expire" . runWithPrefix_ key $ \prefKey -> Hedis.expire prefKey (toInteger expirationTime)

setNxExpireValue :: (ToJSON a, HedisFlow m env) => Text -> ExpirationTime -> a -> m Bool
setNxExpireValue key expirationTime val = withTimeRedis "RedisCluster" "setNxExpire" $ do
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
tryLockRedis key timeout = setNxExpireValue (buildLockResourceName key) timeout ()

unlockRedis :: HedisFlow m env => Text -> m ()
unlockRedis key = void . delValue $ buildLockResourceName key

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

buildLockResourceName :: (IsString a) => Text -> a
buildLockResourceName key = fromString $ "mobility:locker:" <> Text.unpack key

hSetExpValue :: (ToJSON a, HedisFlow m env) => Text -> Text -> a -> ExpirationTime -> m ()
hSetExpValue key field value expirationTime = withLogTag "Redis" $ do
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

hGetValue :: (FromJSON a, HedisFlow m env) => Text -> Text -> m (Maybe a)
hGetValue key field =
  withTimeRedis "RedisCluster" "hGet" $ do
    maybeBS <- runWithPrefix key (`Hedis.hget` cs field)
    case maybeBS of
      Nothing -> pure Nothing
      Just bs -> Error.fromMaybeM (HedisDecodeError $ cs bs) $ Ae.decode $ BSL.fromStrict bs

hDelValue :: HedisFlow m env => Text -> [Text] -> m ()
hDelValue key fields = withTimeRedis "RedisCluster" "hDel" $ runWithPrefix_ key (`Hedis.hdel` map cs fields)

hGetAllValues :: (FromJSON a, HedisFlow m env) => Text -> m [(Text, a)]
hGetAllValues key = withTimeRedis "RedisCluster" "hGetAll" $ do
  hMap <- runWithPrefix key Hedis.hgetall
  pure $ mapMaybe (\(k, val) -> (cs k,) <$> Ae.decode (BSL.fromStrict val)) hMap

zAddExpValue :: (ToJSON Integer, HedisFlow m env) => Text -> Text -> Integer -> ExpirationTime -> m ()
zAddExpValue key field value expirationTime = withLogTag "Redis" $ do
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

zrevrangeWithscoresValue :: (HedisFlow m env) => Text -> Integer -> Integer -> m [(Text, Double)]
zrevrangeWithscoresValue key start stop = do
  res <- runWithPrefix key $ \prefKey ->
    Hedis.zrevrangeWithscores prefKey start stop
  pure $ map (\(k, score) -> (cs' k, score)) res
  where
    cs' :: BS.ByteString -> Text
    cs' = DE.decodeUtf8

zScoreValue :: (FromJSON Double, HedisFlow m env) => Text -> Text -> m (Maybe Double)
zScoreValue key member = do
  maybeZS <- runWithPrefix key (`Hedis.zscore` cs member)
  pure maybeZS

zRevRankValue :: (FromJSON Integer, HedisFlow m env) => Text -> Text -> m (Maybe Integer)
zRevRankValue key member = do
  maybeZr <- runWithPrefix key (`Hedis.zrevrank` cs member)
  pure maybeZr

zCardValue :: (HedisFlow m env) => Text -> m Integer
zCardValue key = runWithPrefix key Hedis.zcard

----------------- Redis Queries for Two Keys ---------------------------------------

buildKeyWithPrefix :: (MonadReader env m, HasEnvPrefix env) => Text -> m Text
buildKeyWithPrefix key = do
  prefix <- asks (.envPrefix)
  pure $ prefix <> ":" <> key

withTwoKeysVoid :: (MonadReader env m, HasEnvPrefix env) => (Text -> m ()) -> Text -> m ()
withTwoKeysVoid func key = do
  _ <- func key
  newKey <- buildKeyWithPrefix key
  func newKey

withTwoKeysMaybe :: (MonadReader env m, HasEnvPrefix env) => (Text -> m (Maybe b)) -> Text -> m (Maybe b)
withTwoKeysMaybe func key = do
  newKey <- buildKeyWithPrefix key
  result <- func newKey
  case result of
    Just _ -> pure result
    Nothing -> func key

withTwoKeysList :: (MonadReader env m, HasEnvPrefix env) => (Text -> m [b]) -> Text -> m [b]
withTwoKeysList func key = do
  newKey <- buildKeyWithPrefix key
  result <- func newKey
  if null result
    then func key
    else pure result

{-
  Redis does not support multiple keys for a single value.
  Therefore, we will use the following logic.
  We will create a uuid for one value for each value. The same uuid will be written for keys with and without prefix.
  When we work with a value, we will query that key first and then edit the value by uuid.
-}

getUUID :: HedisFlow m env => Text -> m (Maybe Text)
getUUID key = do
  keyWithPrefix <- buildKeyWithPrefix key
  mbUuidKey <- getValue keyWithPrefix
  case mbUuidKey of
    Just _ -> return mbUuidKey
    Nothing -> do
      getValue key

get :: (FromJSON a, HedisFlow m env) => Text -> m (Maybe a)
get key = do
  mbUuidKey <- getUUID key
  case mbUuidKey of
    Just uuidKey -> getValue uuidKey
    Nothing -> pure Nothing

get' ::
  (FromJSON a, HedisFlow m env) => Text -> m () -> m (Maybe a)
get' key decodeErrHandler = do
  keyWithPrefix <- buildKeyWithPrefix key
  mbUuidKey <- getValue' keyWithPrefix decodeErrHandler
  case mbUuidKey of
    Just uuidKey -> getValue' uuidKey decodeErrHandler
    Nothing -> do
      mbUuidKeyWithoutPrefix <- getValue' key decodeErrHandler
      case mbUuidKeyWithoutPrefix of
        Just uuidKey -> getValue' uuidKey decodeErrHandler
        Nothing -> return Nothing

safeGet ::
  (FromJSON a, HedisFlow m env) => Text -> m (Maybe a)
safeGet key = get' key (del key)

set ::
  (ToJSON a, HedisFlow m env, MonadGuid m) => Text -> a -> m ()
set key val = do
  uuid <- generateGUIDText
  keyWithPrefix <- buildKeyWithPrefix key
  setValue key uuid
  setValue keyWithPrefix uuid
  setValue uuid val

setExp ::
  (ToJSON a, HedisFlow m env, MonadGuid m) => Text -> a -> ExpirationTime -> m ()
setExp key val expirationTime = do
  uuid <- generateGUIDText
  keyWithPrefix <- buildKeyWithPrefix key
  setExpValue key uuid expirationTime
  setExpValue keyWithPrefix uuid expirationTime
  setExpValue uuid val expirationTime

setNx ::
  (ToJSON a, HedisFlow m env, MonadGuid m) => Text -> a -> m Bool
setNx key val = do
  uuid <- generateGUIDText
  keyWithPrefix <- buildKeyWithPrefix key
  void $ setNxValue key uuid
  void $ setNxValue keyWithPrefix uuid
  setNxValue uuid val

del :: (HedisFlow m env) => Text -> m ()
del key = do
  keyWithPrefix <- buildKeyWithPrefix key
  mbUuid <- getUUID key
  delValue key
  delValue keyWithPrefix
  forM_ mbUuid delValue

rPushExp :: (HedisFlow m env, ToJSON a, MonadGuid m) => Text -> [a] -> ExpirationTime -> m ()
rPushExp key list ex = do
  mbUuid <- getUUID key
  uuid <- case mbUuid of
    Just uuid' -> pure uuid'
    _ -> generateGUIDText
  keyWithPrefix <- buildKeyWithPrefix key
  void $ setNxExpireValue key ex uuid
  void $ setNxExpireValue keyWithPrefix ex uuid
  rPushExpValue uuid list ex

lPush :: (HedisFlow m env, ToJSON a) => Text -> NonEmpty a -> m ()
lPush key list = do
  mbUuid <- getUUID key
  uuid <- case mbUuid of
    Just uuid' -> pure uuid'
    _ -> generateGUIDText
  keyWithPrefix <- buildKeyWithPrefix key
  void $ setNx key uuid
  void $ setNx keyWithPrefix uuid
  lPushValue uuid list

rPush :: (HedisFlow m env, ToJSON a) => Text -> NonEmpty a -> m ()
rPush key list = do
  mbUuid <- getUUID key
  uuid <- case mbUuid of
    Just uuid' -> pure uuid'
    _ -> generateGUIDText
  keyWithPrefix <- buildKeyWithPrefix key
  void $ setNx key uuid
  void $ setNx keyWithPrefix uuid
  rPushValue uuid list

rPop :: (HedisFlow m env, FromJSON a) => Text -> m (Maybe a)
rPop key = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> rPopValue uuid
    Nothing -> pure Nothing

lTrim :: (HedisFlow m env) => Text -> Integer -> Integer -> m ()
lTrim key start stop = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> lTrimValue uuid start stop
    Nothing -> pure ()

clearList :: (HedisFlow m env) => Text -> m ()
clearList key = lTrim key 2 1

lLen :: (HedisFlow m env) => Text -> m Integer
lLen key = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> lLenValue uuid
    Nothing -> pure 0

lRange :: (HedisFlow m env, FromJSON a) => Text -> Integer -> Integer -> m [a]
lRange key start stop = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> lRangeValue uuid start stop
    Nothing -> pure []

getList :: (HedisFlow m env, FromJSON a) => Text -> m [a]
getList key = lRange key 0 (-1)

incr :: (HedisFlow m env) => Text -> m Integer
incr key = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> incrValue uuid
    Nothing -> do
      uuid <- generateGUIDText
      keyWithPrefix <- buildKeyWithPrefix key
      setValue key uuid
      setValue keyWithPrefix uuid
      incrValue uuid

incrby :: (HedisFlow m env) => Text -> Integer -> m Integer
incrby key val = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> incrValueby uuid val
    Nothing -> do
      uuid <- generateGUIDText
      keyWithPrefix <- buildKeyWithPrefix key
      setValue key uuid
      setValue keyWithPrefix uuid
      incrValueby uuid val

decr :: (HedisFlow m env) => Text -> m Integer
decr key = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> decrValue uuid
    Nothing -> do
      uuid <- generateGUIDText
      keyWithPrefix <- buildKeyWithPrefix key
      setValue key uuid
      setValue keyWithPrefix uuid
      decrValue uuid

decrby :: (HedisFlow m env) => Text -> Integer -> m Integer
decrby key val = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> decrValueby uuid val
    Nothing -> do
      uuid <- generateGUIDText
      keyWithPrefix <- buildKeyWithPrefix key
      setValue key uuid
      setValue keyWithPrefix uuid
      decrValueby uuid val

incrByFloat :: (HedisFlow m env) => Text -> Double -> m Double
incrByFloat key toAdd = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> incrValueByFloat uuid toAdd
    Nothing -> do
      uuid <- generateGUIDText
      keyWithPrefix <- buildKeyWithPrefix key
      setValue key uuid
      setValue keyWithPrefix uuid
      incrValueByFloat uuid toAdd

expire :: (HedisFlow m env) => Text -> ExpirationTime -> m ()
expire key expirationTime = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> do
      keyWithPrefix <- buildKeyWithPrefix key
      expireValue uuid expirationTime
      expireValue key expirationTime
      expireValue keyWithPrefix expirationTime
    Nothing -> pure ()

setNxExpire :: (ToJSON a, HedisFlow m env) => Text -> ExpirationTime -> a -> m Bool
setNxExpire key expirationTime val = do
  uuid <- generateGUIDText
  keyWithPrefix <- buildKeyWithPrefix key
  void $ setNxExpireValue key expirationTime uuid
  void $ setNxExpireValue keyWithPrefix expirationTime uuid
  setNxExpireValue uuid expirationTime val

hSetExp :: (ToJSON a, HedisFlow m env) => Text -> Text -> a -> ExpirationTime -> m ()
hSetExp key field val expirationTime = do
  uuid <- generateGUIDText
  keyWithPrefix <- buildKeyWithPrefix key
  setExpValue key uuid expirationTime
  setExpValue keyWithPrefix uuid expirationTime
  hSetExpValue uuid field val expirationTime

hGet :: (FromJSON a, HedisFlow m env) => Text -> Text -> m (Maybe a)
hGet key field = do
  mbUuid <- getUUID key
  case mbUuid of
    Nothing -> pure Nothing
    Just uuid -> hGetValue uuid field

hDel :: HedisFlow m env => Text -> [Text] -> m ()
hDel key fields = do
  mbUuid <- getUUID key
  keyWithPrefix <- buildKeyWithPrefix key
  forM_ mbUuid (`hDelValue` fields)
  delValue key
  delValue keyWithPrefix

hGetAll :: (FromJSON a, HedisFlow m env) => Text -> m [(Text, a)]
hGetAll key = do
  mbUuid <- getUUID key
  case mbUuid of
    Nothing -> pure []
    Just uuid -> hGetAllValues uuid

zAddExp :: (ToJSON Integer, HedisFlow m env) => Text -> Text -> Integer -> ExpirationTime -> m ()
zAddExp key field value expirationTime = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> zAddExpValue uuid field value expirationTime
    Nothing -> do
      uuid <- generateGUIDText
      keyWithPrefix <- buildKeyWithPrefix key
      setExpValue key uuid expirationTime
      setExpValue keyWithPrefix uuid expirationTime
      zAddExpValue uuid field value expirationTime

zrevrangeWithscores :: (HedisFlow m env) => Text -> Integer -> Integer -> m [(Text, Double)]
zrevrangeWithscores key start stop = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> zrevrangeWithscoresValue uuid start stop
    Nothing -> pure []

zScore :: (FromJSON Double, HedisFlow m env) => Text -> Text -> m (Maybe Double)
zScore key member = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> zScoreValue uuid member
    Nothing -> pure Nothing

zRevRank :: (FromJSON Integer, HedisFlow m env) => Text -> Text -> m (Maybe Integer)
zRevRank key member = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> zRevRankValue uuid member
    Nothing -> pure Nothing

zCard :: (HedisFlow m env) => Text -> m Integer
zCard key = do
  mbUuid <- getUUID key
  case mbUuid of
    Just uuid -> zCardValue uuid
    Nothing -> pure 0
