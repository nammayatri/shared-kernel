module Kernel.Storage.InMem.Management.Handler where

import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, writeIORef)
import Data.List (sort)
import qualified Data.Text as T
import Data.Time (timeToTimeOfDay, utctDayTime)
import Kernel.Prelude
import Kernel.Storage.Hedis.Config (HedisFlow)
import Kernel.Storage.Hedis.Queries (runInMultiCloudRedisWrite, setExp)
import Kernel.Storage.InMem.Management.Types
import Kernel.Types.App (MonadFlow)
import Kernel.Types.CacheFlow
import Kernel.Types.Error (AuthError (..), GenericError (InternalError))
import Kernel.Types.TryException (TryException)
import Kernel.Utils.Error.Throwing (throwError)
import Kernel.Utils.Time (getCurrentTime)
import qualified System.Environment as Se

validateInMemToken :: (MonadFlow m, MonadReader r m, HasInMemEnv r) => Maybe Text -> m ()
validateInMemToken mbToken = do
  inMemEnv <- asks (.inMemEnv)
  case inMemManagementToken inMemEnv of
    Nothing -> throwError $ InternalError "INMEM_MANAGEMENT_TOKEN not configured"
    Just expected ->
      case mbToken of
        Nothing -> throwError Unauthorized
        Just token ->
          unless (token == expected) $
            throwError Unauthorized

getKeys ::
  (MonadFlow m, MonadReader r m, HasInMemEnv r) =>
  Maybe Text ->
  -- | Optional substring to filter keys by (case-sensitive infix match).
  Maybe Text ->
  -- | Optional max number of keys to return.
  Maybe Int ->
  -- | Optional number of keys to skip (applied before limit).
  Maybe Int ->
  m InMemKeysResponse
getKeys mbToken mbPattern mbLimit mbOffset = do
  validateInMemToken mbToken
  inMemEnv <- asks (.inMemEnv)
  cacheInfo <- liftIO $ readIORef (inMemHashMap inMemEnv)
  -- Sort so that limit/offset paginate deterministically over a snapshot.
  let matched = sort $ maybe identity (\pat -> filter (pat `T.isInfixOf`)) mbPattern $ HM.keys (cache cacheInfo)
      paginated = maybe identity take mbLimit $ maybe identity drop mbOffset matched
      keysMeta = map (\k -> InMemKeyEntry {keyName = k, keySchema = Nothing}) paginated
  pure InMemKeysResponse {keyList = keysMeta, totalKeys = length matched}

getValue ::
  (MonadFlow m, MonadReader r m, HasInMemEnv r) =>
  Maybe Text ->
  InMemGetRequest ->
  m InMemGetResponse
getValue mbToken req = do
  validateInMemToken mbToken
  inMemEnv <- asks (.inMemEnv)
  cacheInfo <- liftIO $ readIORef (inMemHashMap inMemEnv)
  let mbInfo = HM.lookup req.key (cache cacheInfo)
  pure $ case mbInfo of
    Nothing ->
      InMemGetResponse {found = False, value = Nothing}
    Just keyInfo ->
      InMemGetResponse {found = True, value = Ae.decode (cachedJson keyInfo)}

refreshCache ::
  (MonadFlow m, MonadReader r m, HasInMemEnv r, HedisFlow m r, TryException m) =>
  Maybe Text ->
  InMemRefreshRequest ->
  m InMemRefreshResponse
refreshCache mbToken req = do
  validateInMemToken mbToken
  inMemEnv <- asks (.inMemEnv)
  now <- getCurrentTime
  oldInfo <- liftIO $ readIORef (inMemHashMap inMemEnv)
  let oldCount = HM.size (cache oldInfo)
  (deletedCount, remainingCount) <- case req.keyInfix of
    Nothing -> do
      liftIO $
        writeIORef (inMemHashMap inMemEnv) $
          InMemCacheInfo {cache = HM.empty, cacheSize = 0, createdAt = now}
      pure (oldCount, 0)
    Just infix_ -> do
      let toKeep = HM.filterWithKey (\k _ -> not (infix_ `T.isInfixOf` k)) (cache oldInfo)
          newSize = foldl' (\acc keyInfo -> acc + keyInfo.cacheDataSize) 0 (HM.elems toKeep)
      liftIO $
        writeIORef (inMemHashMap inMemEnv) $
          InMemCacheInfo {cache = toKeep, cacheSize = newSize, createdAt = oldInfo.createdAt}
      pure (HM.size (cache oldInfo) - HM.size toKeep, HM.size toKeep)
  -- Write to Redis forceCleanup key so other pods also clean up
  let cleanupTimeOfDay = timeToTimeOfDay (utctDayTime now)
      val =
        ForceCleanupExpiryValue
          { forceCleanupTimestamp = cleanupTimeOfDay,
            forceCleanupKeyPrefix = req.keyInfix
          }
  runInMultiCloudRedisWrite $ setExp "inmem:force:cleanup:timeofday" val 600
  pure
    InMemRefreshResponse
      { deletedKeys = deletedCount,
        remainingKeys = remainingCount,
        redisNotified = True
      }

getServerInfo ::
  (MonadFlow m, MonadReader r m, HasInMemEnv r) =>
  Maybe Text ->
  m InMemServerInfoResponse
getServerInfo mbToken = do
  validateInMemToken mbToken
  inMemEnv <- asks (.inMemEnv)
  let svcName = fromMaybe "unknown" (inMemServiceName inMemEnv)
  podName' <- liftIO $ Se.lookupEnv "POD_NAME"
  pure
    InMemServerInfoResponse
      { serviceName = svcName,
        podName = T.pack <$> podName'
      }
