module Kernel.Storage.InMem.Management.Handler where

import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, writeIORef)
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Storage.InMem.Management.Types
import Kernel.Types.App (MonadFlow)
import Kernel.Types.CacheFlow
import Kernel.Utils.Time (getCurrentTime)
import qualified System.Environment as Se

getKeys ::
  (MonadFlow m, MonadReader r m, HasInMemEnv r) =>
  m InMemKeysResponse
getKeys = do
  inMemEnv <- asks (.inMemEnv)
  cacheInfo <- liftIO $ readIORef (inMemHashMap inMemEnv)
  let keysMeta =
        map
          (\(k, _) -> InMemKeyEntry {keyName = k, keySchema = Nothing})
          (HM.toList (cache cacheInfo))
  pure InMemKeysResponse {keyList = keysMeta}

getValue ::
  (MonadFlow m, MonadReader r m, HasInMemEnv r) =>
  InMemGetRequest ->
  m InMemGetResponse
getValue req = do
  inMemEnv <- asks (.inMemEnv)
  cacheInfo <- liftIO $ readIORef (inMemHashMap inMemEnv)
  let mbInfo = HM.lookup req.key (cache cacheInfo)
  pure $ case mbInfo of
    Nothing ->
      InMemGetResponse {found = False, value = Nothing}
    Just _info ->
      -- cachedData is stored as `Any` via unsafeCoerce; we cannot safely
      -- extract a JSON Value without knowing the original type.
      -- Services that need actual value introspection should override this handler.
      InMemGetResponse {found = True, value = Nothing}

refreshCache ::
  (MonadFlow m, MonadReader r m, HasInMemEnv r) =>
  InMemRefreshRequest ->
  m InMemRefreshResponse
refreshCache req = do
  inMemEnv <- asks (.inMemEnv)
  now <- getCurrentTime
  oldInfo <- liftIO $ readIORef (inMemHashMap inMemEnv)
  let oldCount = HM.size (cache oldInfo)
  case req.keyPrefix of
    Nothing -> do
      liftIO $
        writeIORef (inMemHashMap inMemEnv) $
          InMemCacheInfo {cache = HM.empty, cacheSize = 0, createdAt = now}
      pure InMemRefreshResponse {deletedKeys = oldCount, remainingKeys = 0}
    Just prefix -> do
      let toKeep = HM.filterWithKey (\k _ -> not (prefix `T.isPrefixOf` k)) (cache oldInfo)
          newSize = foldl' (\acc info -> acc + info.cacheDataSize) 0 (HM.elems toKeep)
          deletedCount = HM.size (cache oldInfo) - HM.size toKeep
      liftIO $
        writeIORef (inMemHashMap inMemEnv) $
          InMemCacheInfo {cache = toKeep, cacheSize = newSize, createdAt = oldInfo.createdAt}
      pure
        InMemRefreshResponse
          { deletedKeys = deletedCount,
            remainingKeys = HM.size toKeep
          }

getServerInfo ::
  (MonadFlow m, MonadReader r m, HasInMemEnv r) =>
  Text ->
  m InMemServerInfoResponse
getServerInfo svcName = do
  podName' <- liftIO $ Se.lookupEnv "POD_NAME"
  pure
    InMemServerInfoResponse
      { serviceName = svcName,
        podName = T.pack <$> podName'
      }
