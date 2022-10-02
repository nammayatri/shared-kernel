module Beckn.Storage.Hedis.Queries where

import Beckn.Prelude
import Beckn.Storage.Hedis.Config
import Beckn.Storage.Hedis.Error
import Beckn.Utils.Error.Throwing
import Beckn.Utils.Logging
import qualified Data.Aeson as Ae
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.String.Conversions
import qualified Data.Text as Text
import Database.Redis (Queued, Redis, RedisTx, Reply, TxResult (..))
import qualified Database.Redis as Hedis
import GHC.Records.Extra

type ExpirationTime = Int

runHedis ::
  HedisFlow m env => Redis (Either Reply a) -> m a
runHedis action = do
  eithRes <- runHedisEither action
  fromEitherM (HedisReplyError . show) eithRes

runHedisEither ::
  HedisFlow m env => Redis (Either Reply a) -> m (Either Reply a)
runHedisEither action = do
  con <- asks (.hedisEnv.hedisConnection)
  liftIO $ Hedis.runRedis con action

runHedisTransaction ::
  HedisFlow m env => RedisTx (Queued a) -> m a
runHedisTransaction action = do
  con <- asks (.hedisEnv.hedisConnection)
  res <- liftIO . Hedis.runRedis con $ Hedis.multiExec action
  case res of
    TxError err -> throwError $ HedisReplyError err
    TxAborted -> throwError HedisTransactionAborted
    TxSuccess a -> return a

----------------------------------------------------

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

get ::
  (FromJSON a, HedisFlow m env) => Text -> m (Maybe a)
get key = do
  maybeBS <- runWithPrefix key Hedis.get
  case maybeBS of
    Nothing -> pure Nothing
    Just bs -> fromMaybeM (HedisDecodeError $ cs bs) $ Ae.decode $ BSL.fromStrict bs

set ::
  (ToJSON a, HedisFlow m env) => Text -> a -> m ()
set key val = runWithPrefix_ key $ \prefKey ->
  Hedis.set prefKey $ BSL.toStrict $ Ae.encode val

setExp ::
  (ToJSON a, HedisFlow m env) => Text -> a -> ExpirationTime -> m ()
setExp key val expirationTime = do
  prefKey <- buildKey key
  void . runHedisTransaction $ do
    void . Hedis.set prefKey $ BSL.toStrict $ Ae.encode val
    Hedis.expire prefKey (toInteger expirationTime)

setNx ::
  (ToJSON a, HedisFlow m env) => Text -> a -> m Bool
setNx key val = runWithPrefix key $ \prefKey ->
  Hedis.setnx prefKey $ BSL.toStrict $ Ae.encode val

del :: (HedisFlow m env) => Text -> m ()
del key = runWithPrefix_ key $ \prefKey -> Hedis.del [prefKey]

rPushExp :: (HedisFlow m env, ToJSON a) => Text -> [a] -> ExpirationTime -> m ()
rPushExp key list ex = do
  prefKey <- buildKey key
  withLogTag "Redis" $ logDebug $ "working with key : " <> cs prefKey
  unless (null list) $
    void . runHedisTransaction $ do
      void . Hedis.rpush prefKey $ map (BSL.toStrict . Ae.encode) list
      Hedis.expire prefKey (toInteger ex)

lPush :: (HedisFlow m env, ToJSON a) => Text -> NonEmpty a -> m ()
lPush key list = runWithPrefix_ key $ \prefKey ->
  Hedis.lpush prefKey $ map (BSL.toStrict . Ae.encode) (toList list)

rPush :: (HedisFlow m env, ToJSON a) => Text -> NonEmpty a -> m ()
rPush key list = runWithPrefix_ key $ \prefKey ->
  Hedis.rpush prefKey $ map (BSL.toStrict . Ae.encode) (toList list)

rPop :: (HedisFlow m env, FromJSON a) => Text -> m (Maybe a)
rPop key = do
  res <- runWithPrefix key $ \prefKey -> Hedis.rpop prefKey
  pure $ Ae.decode . BSL.fromStrict =<< res

lTrim :: (HedisFlow m env) => Text -> Integer -> Integer -> m ()
lTrim key start stop = runWithPrefix_ key $ \prefKey ->
  Hedis.ltrim prefKey start stop

clearList :: (HedisFlow m env) => Text -> m ()
clearList key = lTrim key 2 1

lLen :: (HedisFlow m env) => Text -> m Integer
lLen key = runWithPrefix key Hedis.llen

lRange :: (HedisFlow m env, FromJSON a) => Text -> Integer -> Integer -> m [a]
lRange key start stop = do
  res <- runWithPrefix key $ \prefKey ->
    Hedis.lrange prefKey start stop
  mapM (\a -> fromMaybeM (HedisDecodeError $ cs a) . Ae.decode $ cs a) res

getList :: (HedisFlow m env, FromJSON a) => Text -> m [a]
getList key = lRange key 0 (-1)

incr :: (HedisFlow m env) => Text -> m Integer
incr key = runWithPrefix key Hedis.incr

incrByFloat :: (HedisFlow m env) => Text -> Double -> m Double
incrByFloat key toAdd = runWithPrefix key $ \prefKey ->
  Hedis.incrbyfloat prefKey toAdd

expire :: (HedisFlow m env) => Text -> ExpirationTime -> m ()
expire key expirationTime = runWithPrefix_ key $ \prefKey ->
  Hedis.expire prefKey (toInteger expirationTime)

setNxExpire :: (ToJSON a, HedisFlow m env) => Text -> ExpirationTime -> a -> m Bool
setNxExpire key expirationTime val = do
  eithRes <- runWithPrefixEither key $ \prefKey ->
    Hedis.setOpts prefKey (cs $ Ae.encode val) $
      Hedis.SetOpts (Just $ toInteger expirationTime) Nothing (Just Hedis.Nx)
  pure $ case eithRes of
    Right Hedis.Ok -> True
    _ -> False

delByPattern :: HedisFlow m env => Text -> m ()
delByPattern ptrn = do
  runWithPrefix_ ptrn $ \prefKey ->
    Hedis.eval @_ @_ @Reply "for i, name in ipairs(redis.call('KEYS', ARGV[1])) do redis.call('DEL', name); end" ["0"] [prefKey]

tryLockRedis :: HedisFlow m env => Text -> ExpirationTime -> m Bool
tryLockRedis key timeout = setNxExpire (buildLockResourceName key) timeout ()

unlockRedis :: HedisFlow m env => Text -> m ()
unlockRedis key = void . del $ buildLockResourceName key

buildLockResourceName :: (IsString a) => Text -> a
buildLockResourceName key = fromString $ "beckn:locker:" <> Text.unpack key
