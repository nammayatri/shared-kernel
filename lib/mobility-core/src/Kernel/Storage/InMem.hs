{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.Storage.InMem where

import qualified Crypto.Hash as Hash
import qualified Data.Aeson as Ae
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.String.Conversions
import qualified Data.Text as T
import Data.Time (timeOfDayToTime, timeToTimeOfDay, utctDayTime)
import Data.Typeable
import Database.Redis as Hedis
import EulerHS.Prelude
import Kernel.Storage.Hedis.Config
import Kernel.Storage.Hedis.Queries (runInMultiCloudRedisWrite, setExp)
import Kernel.Storage.InMem.Management.SidecarClient (callRefresh, callRegisterKey)
import Kernel.Storage.InMem.Management.Types (RegisterKeyRequest (..), SidecarRefreshRequest (..))
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.App (MonadFlow)
import Kernel.Types.CacheFlow
import Kernel.Types.TryException (TryException)
import Kernel.Utils.DatastoreLatencyCalculator
import Kernel.Utils.Time (Seconds, UTCTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime, threadDelaySec)
import qualified Network.HTTP.Client as HTTP
import Servant.Client (parseBaseUrl)
import qualified System.Environment as Se
import Text.Hex (encodeHex)
import Unsafe.Coerce (unsafeCoerce)

defaultInMemCacheInfo :: UTCTime -> InMemCacheInfo
defaultInMemCacheInfo now = InMemCacheInfo {cache = mempty, cacheSize = 0, createdAt = now}

headMay :: [x] -> Maybe x
headMay [] = Nothing
headMay (x : _xs) = Just x

withInMemCache :: forall b r m. (Show b, Ae.ToJSON b, MonadFlow m, MonadReader r m, HasInMemEnv r, Typeable b, CoreMetrics m) => [Text] -> Seconds -> m b -> m b
withInMemCache cacheKeys ttlInSeconds fn = fmap fst . withTimeGeneric ("InMem-Fetch" <> show (headMay cacheKeys)) $ do
  inMemEnv <- asks (.inMemEnv)
  if inMemEnv.enableInMem && ttlInSeconds > 0
    then do
      let key = cs . T.intercalate (":" :: Text) $ cacheKeys
      let keyType = show $ typeRep (Proxy :: Proxy b)
      -- Adding response type to the key to help reduce chances of collisions.
      let mbCacheKey
            | length key > 200 = do
              let hashedKey = BA.convert @(Hash.Digest Hash.SHA256) $ Hash.hashlazy (encodeUtf8 key)
              Just $ encodeHex hashedKey <> keyType
            | length key < 2 = Nothing
            | otherwise = Just $ key <> keyType
      case mbCacheKey of
        Nothing -> fn
        Just cacheKey -> do
          let inMemCache = inMemHashMap inMemEnv
          mbRes <- HM.lookup cacheKey . cache <$> readIORef inMemCache
          now <- getCurrentTime
          case mbRes of
            Just resAny -> do
              if addUTCTime (fromIntegral resAny.ttlInSeconds) resAny.createdAt > now
                then pure (unsafeCoerce (cachedData resAny))
                else recache inMemEnv inMemCache now cacheKey
            Nothing -> recache inMemEnv inMemCache now cacheKey
    else fn
  where
    recache env inMemCache now cacheKey = do
      res <- fn
      let sizeOfRes = sizeOf res
      let encodedRes = Ae.encode res
      void $ atomicModifyIORef inMemCache (\old -> (old {cache = HM.insert cacheKey (InMemKeyInfo {lastUsed = now, cachedData = unsafeCoerce @_ @Any res, cachedJson = encodedRes, cacheDataSize = sizeOfRes, createdAt = now, ttlInSeconds = ttlInSeconds}) (cache old), cacheSize = (cacheSize old) + sizeOfRes}, ()))
      registerKeyWithSidecar env cacheKey ttlInSeconds
      pure res
    sizeOf :: Show b => b -> Bytes
    sizeOf a = fromIntegral . length $ (show :: b -> Text) a

registerKeyWithSidecar :: MonadIO m => InMemEnv -> Text -> Seconds -> m ()
registerKeyWithSidecar env cacheKey cacheTtl =
  case inMemSidecarEnv env of
    Nothing -> pure ()
    Just sidecar -> liftIO $
      void $
        forkIO $ do
          let req = RegisterKeyRequest {keyName = cacheKey, keySchema = Nothing, ttlInSeconds = Just cacheTtl}
          void (callRegisterKey (sidecarManager sidecar) (sidecarBaseUrl sidecar) req)
            `catch` (\(_ :: SomeException) -> pure ())

refreshInMem :: (MonadFlow m, MonadReader r m, HasInMemEnv r, HedisFlow m r, TryException m) => Text -> m ()
refreshInMem keyInfix = do
  inMemEnv <- asks (.inMemEnv)
  now <- getCurrentTime
  liftIO $
    atomicModifyIORef (inMemHashMap inMemEnv) $ \old ->
      let toKeep = HM.filterWithKey (\k _ -> not (keyInfix `T.isInfixOf` k)) (cache old)
          newSize = foldl' (\acc infoa -> acc + infoa.cacheDataSize) 0 (HM.elems toKeep)
       in (InMemCacheInfo {cache = toKeep, cacheSize = newSize, createdAt = old.createdAt}, ())
  -- Write to Redis forceCleanup key so other pods also clean up
  let cleanupTimeOfDay = timeToTimeOfDay (utctDayTime now)
      val =
        ForceCleanupExpiryValue
          { forceCleanupTimestamp = cleanupTimeOfDay,
            forceCleanupKeyPrefix = Just keyInfix
          }
  runInMultiCloudRedisWrite $ setExp "inmem:force:cleanup:timeofday" val 600
  -- Call sidecar to propagate refresh to all pods
  case (inMemSidecarEnv inMemEnv, inMemServiceName inMemEnv) of
    (Just sidecar, Just svcName) -> liftIO $ do
      let req = SidecarRefreshRequest {serviceName = svcName, keyInfix = keyInfix}
      void (callRefresh (sidecarManager sidecar) (sidecarBaseUrl sidecar) req)
        `catch` (\(_ :: SomeException) -> pure ())
    _ -> pure ()

inMemCleanupThread :: Maybe HedisEnv -> InMemEnv -> IO ()
inMemCleanupThread mbHedisEnv inMemEnv = do
  let inMemCache = inMemEnv.inMemHashMap
      maxInMemSize = inMemEnv.maxInMemSize
  inMemCacheInfo <- readIORef inMemCache
  now <- getCurrentTime
  -- Step 1: Remove all TTL-expired keys
  let isExpired keyInfo = addUTCTime (secondsToNominalDiffTime keyInfo.ttlInSeconds) keyInfo.createdAt < now
      afterTtlCleanup = HM.filter (not . isExpired) (cache inMemCacheInfo)
      afterTtlSize = foldl' (\acc keyInfo -> acc + keyInfo.cacheDataSize) 0 (HM.elems afterTtlCleanup)
      ttlCleanupHappened = HM.size afterTtlCleanup < HM.size (cache inMemCacheInfo)
  -- Step 2: Size-based eviction (LRU) if still over capacity
  let sizeBasedCleanupRequired = afterTtlSize > maxInMemSize
  let updatedCacheInfo =
        if sizeBasedCleanupRequired
          then do
            let cacheList = reverse $ sortOn (\(_, InMemKeyInfo {lastUsed}) -> lastUsed) $ HM.toList afterTtlCleanup
                targetSize = floor $ (fromIntegral maxInMemSize * 0.75 :: Double)
                (updatedCacheSize, updatedCache) =
                  foldl'
                    ( \(acc, accCacheList) (k, v@(InMemKeyInfo {cacheDataSize})) ->
                        if cacheDataSize + acc <= targetSize
                          then (acc + cacheDataSize, accCacheList ++ [(k, v)])
                          else (acc, accCacheList)
                    )
                    (0, [])
                    cacheList
            InMemCacheInfo {cache = HM.fromList updatedCache, cacheSize = updatedCacheSize, createdAt = inMemCacheInfo.createdAt}
          else InMemCacheInfo {cache = afterTtlCleanup, cacheSize = afterTtlSize, createdAt = inMemCacheInfo.createdAt}
  (forceCleanup, finalCacheInfo) <-
    case mbHedisEnv of
      Just hedisEnv -> do
        let key = cs $ hedisEnv.keyModifier "inmem:force:cleanup:timeofday"
        forceCleanupValue <- Hedis.runRedis hedisEnv.hedisConnection (Hedis.get key)
        case forceCleanupValue of
          Left err -> do
            print err
            pure (False, updatedCacheInfo)
          Right forceCleanupVal -> do
            let forceCleanupExpiryValue :: Maybe ForceCleanupExpiryValue = Ae.decode . BSL.fromStrict =<< forceCleanupVal
            case forceCleanupExpiryValue of
              Just cacheExpiryValue -> do
                let cacheExpiryTime = timeOfDayToTime (forceCleanupTimestamp cacheExpiryValue)
                let createAtTime = utctDayTime updatedCacheInfo.createdAt
                if createAtTime < cacheExpiryTime
                  then do
                    void $ Hedis.runRedis hedisEnv.hedisConnection $ Hedis.expire key 600 -- do that it doesn't happen daily once user adds the key and forgets to set expiry
                    case (forceCleanupKeyPrefix cacheExpiryValue) of
                      Just cleanupKeyPrefix -> do
                        let cacheList :: [(Text, InMemKeyInfo)] = HM.toList updatedCacheInfo.cache
                            (updatedCacheSize, updatedCache) =
                              foldl'
                                ( \(acc, accCacheList) (k, v@(InMemKeyInfo {cacheDataSize})) ->
                                    if cleanupKeyPrefix `T.isInfixOf` k
                                      then (acc, accCacheList)
                                      else (acc + cacheDataSize, accCacheList ++ [(k, v)])
                                )
                                (0, [])
                                cacheList
                        pure (True, InMemCacheInfo {cache = HM.fromList updatedCache, cacheSize = updatedCacheSize, createdAt = inMemCacheInfo.createdAt})
                      Nothing -> pure (True, defaultInMemCacheInfo now)
                  else do
                    pure (False, updatedCacheInfo)
              Nothing -> do
                pure (False, updatedCacheInfo)
      Nothing -> do
        pure (False, updatedCacheInfo)
  when (ttlCleanupHappened || sizeBasedCleanupRequired || forceCleanup) $
    writeIORef inMemCache finalCacheInfo
  threadDelaySec $ 60

setupInMemEnv :: InMemConfig -> Maybe HedisEnv -> IO InMemEnv
setupInMemEnv inMemConfig mbHedisEnv = do
  now <- getCurrentTime
  mbSidecarEnv <- initSidecarEnv
  mgmtToken <- fmap T.pack <$> Se.lookupEnv "INMEM_MANAGEMENT_TOKEN"
  svcName <- fmap T.pack <$> Se.lookupEnv "INMEM_SERVICE_NAME"
  if inMemConfig.enableInMem
    then do
      let inMemCacheInfo = defaultInMemCacheInfo now
      inMemHashMap <- newIORef inMemCacheInfo
      let inMemEnv =
            InMemEnv
              { enableInMem = inMemConfig.enableInMem,
                maxInMemSize = inMemConfig.maxInMemSize,
                inMemHashMap = inMemHashMap,
                inMemSidecarEnv = mbSidecarEnv,
                inMemManagementToken = mgmtToken,
                inMemServiceName = svcName
              }
      void $ forkIO $ forever $ inMemCleanupThread mbHedisEnv inMemEnv
      pure inMemEnv
    else do
      inMemHashMap <- newIORef $ defaultInMemCacheInfo now
      pure $ InMemEnv {enableInMem = False, maxInMemSize = 0, inMemHashMap, inMemSidecarEnv = mbSidecarEnv, inMemManagementToken = mgmtToken, inMemServiceName = svcName}

initSidecarEnv :: IO (Maybe InMemSidecarEnv)
initSidecarEnv = do
  mbUrl <- Se.lookupEnv "INMEM_SIDECAR_URL"
  case mbUrl of
    Nothing -> pure Nothing
    Just urlStr -> do
      baseUrl <- parseBaseUrl urlStr
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      pure $ Just InMemSidecarEnv {sidecarBaseUrl = baseUrl, sidecarManager = manager}
