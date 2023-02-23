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
import qualified Data.Text as Text
import Database.Redis (Queued, Redis, RedisTx, Reply, TxResult (..))
import qualified Database.Redis as Hedis
import GHC.Records.Extra
import Kernel.Prelude
import Kernel.Storage.Hedis.Config
import Kernel.Storage.Hedis.Error
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
  con <- asks (.hedisEnv.hedisConnection)
  liftIO $ Hedis.runRedis con action

runHedisTransaction ::
  HedisFlow m env => RedisTx (Queued a) -> m a
runHedisTransaction action = do
  con <- asks (.hedisEnv.hedisConnection)
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
  local (\env -> env{hedisEnv = env.hedisEnv{keyModifier = identity}}) f

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
    Just bs -> Error.fromMaybeM (HedisDecodeError $ cs bs) $ Ae.decode $ BSL.fromStrict bs

get' ::
  (FromJSON a, HedisFlow m env) => Text -> m () -> m (Maybe a)
get' key decodeErrHandler = do
  maybeBS <- runWithPrefix key Hedis.get
  case maybeBS of
    Nothing -> pure Nothing
    Just bs -> do
      case Ae.decode $ BSL.fromStrict bs of
        Just a -> return $ Just a
        Nothing -> do
          withLogTag "Redis" $ logDebug $ "Decode Failure for the key " <> key
          decodeErrHandler
          return Nothing

safeGet ::
  (FromJSON a, HedisFlow m env) => Text -> m (Maybe a)
safeGet key = get' key (del key)

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
  mapM (\a -> Error.fromMaybeM (HedisDecodeError $ cs a) . Ae.decode $ cs a) res

getList :: (HedisFlow m env, FromJSON a) => Text -> m [a]
getList key = lRange key 0 (-1)

incr :: (HedisFlow m env) => Text -> m Integer
incr key = runWithPrefix key Hedis.incr

incrby :: (HedisFlow m env) => Text -> Integer -> m Integer
incrby key val = runWithPrefix key $ flip Hedis.incrby val

decr :: (HedisFlow m env) => Text -> m Integer
decr key = runWithPrefix key Hedis.decr

decrby :: (HedisFlow m env) => Text -> Integer -> m Integer
decrby key val = runWithPrefix key $ flip Hedis.decrby val

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

hSetExp :: (ToJSON a, HedisFlow m env) => Text -> Text -> a -> ExpirationTime -> m ()
hSetExp key field value expirationTime = do
  prefKey <- buildKey key
  void . runHedisTransaction $ do
    void . Hedis.hset prefKey (cs field) $ BSL.toStrict $ Ae.encode value
    Hedis.expire prefKey (toInteger expirationTime)

hGet :: (FromJSON a, HedisFlow m env) => Text -> Text -> m (Maybe a)
hGet key field = do
  maybeBS <- runWithPrefix key (`Hedis.hget` cs field)
  case maybeBS of
    Nothing -> pure Nothing
    Just bs -> Error.fromMaybeM (HedisDecodeError $ cs bs) $ Ae.decode $ BSL.fromStrict bs

hDel :: HedisFlow m env => Text -> [Text] -> m ()
hDel key fields = runWithPrefix_ key (`Hedis.hdel` map cs fields)

hGetAll :: (FromJSON a, HedisFlow m env) => Text -> m [(Text, a)]
hGetAll key = do
  hMap <- runWithPrefix key Hedis.hgetall
  pure $ mapMaybe (\(k, val) -> (cs k,) <$> Ae.decode (BSL.fromStrict val)) hMap
