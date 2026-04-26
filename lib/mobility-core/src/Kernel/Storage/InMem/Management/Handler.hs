module Kernel.Storage.InMem.Management.Handler where

import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, writeIORef)
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Storage.InMem.Management.Types
import Kernel.Types.App (MonadFlow)
import Kernel.Types.CacheFlow
import Kernel.Types.Error (AuthError (..), GenericError (InternalError))
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
  m InMemKeysResponse
getKeys mbToken = do
  validateInMemToken mbToken
  inMemEnv <- asks (.inMemEnv)
  cacheInfo <- liftIO $ readIORef (inMemHashMap inMemEnv)
  let keysMeta =
        map
          (\(k, _) -> InMemKeyEntry {keyName = k, keySchema = Nothing})
          (HM.toList (cache cacheInfo))
  pure InMemKeysResponse {keyList = keysMeta}

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
    Just _info ->
      -- cachedData is stored as `Any` via unsafeCoerce; we cannot safely
      -- extract a JSON Value without knowing the original type.
      -- Services that need actual value introspection should override this handler.
      InMemGetResponse {found = True, value = Nothing}

refreshCache ::
  (MonadFlow m, MonadReader r m, HasInMemEnv r) =>
  Maybe Text ->
  InMemRefreshRequest ->
  m InMemRefreshResponse
refreshCache mbToken req = do
  validateInMemToken mbToken
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
  Maybe Text ->
  Text ->
  m InMemServerInfoResponse
getServerInfo mbToken svcName = do
  validateInMemToken mbToken
  podName' <- liftIO $ Se.lookupEnv "POD_NAME"
  pure
    InMemServerInfoResponse
      { serviceName = svcName,
        podName = T.pack <$> podName'
      }
