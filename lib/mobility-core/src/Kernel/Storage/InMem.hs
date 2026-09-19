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
import qualified Data.HashSet as HS
import Data.String.Conversions
import qualified Data.Text as T
import Data.Time (timeToTimeOfDay, utctDayTime)
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

-- | Bounded metric label: only the key's category (first two ':'-separated
-- segments). Full keys carry entity UUIDs — labelling by them mints one
-- permanent time-series per key and grows the registry without bound.
inMemMetricLabel :: [Text] -> Text
inMemMetricLabel [] = "InMem-Fetch:empty"
inMemMetricLabel (k : _) = "InMem-Fetch:" <> T.intercalate ":" (take 2 (T.splitOn ":" k))

withInMemCache :: forall b r m. (Ae.ToJSON b, MonadFlow m, MonadReader r m, HasInMemEnv r, Typeable b, CoreMetrics m) => [Text] -> Seconds -> m b -> m b
withInMemCache cacheKeys ttlInSeconds fn = fmap fst . withTimeGeneric (inMemMetricLabel cacheKeys) $ do
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
                then do
                  -- Touch lastUsed at most once per 10s per key: LRU eviction
                  -- needs real usage info, but a write per read would make
                  -- every hit contend on the shared IORef.
                  when (addUTCTime 10 resAny.lastUsed < now) $
                    void $
                      atomicModifyIORef' inMemCache $ \cur ->
                        (cur {cache = HM.adjust (\(i :: InMemKeyInfo) -> i {lastUsed = now}) cacheKey (cache cur)}, ())
                  pure (unsafeCoerce (cachedData resAny))
                else recache inMemEnv inMemCache now cacheKey
            Nothing -> recache inMemEnv inMemCache now cacheKey
    else fn
  where
    recache env inMemCache now cacheKey = do
      res <- fn
      -- Size from the JSON we store anyway; the old sizeOf rendered the whole
      -- value with 'show' just to count characters — a second full
      -- serialization of a fat config object on every miss.
      let encodedRes = Ae.encode res
          sizeOfRes = fromIntegral (BSL.length encodedRes)
          keyInfo = InMemKeyInfo {lastUsed = now, cachedData = unsafeCoerce @_ @Any res, cachedJson = encodedRes, cacheDataSize = sizeOfRes, createdAt = now, ttlInSeconds = ttlInSeconds}
      void $
        atomicModifyIORef' inMemCache $ \old ->
          let newCache = HM.insert cacheKey keyInfo (cache old)
           in -- seq: the records are lazy, so without it the insert stays an
              -- unevaluated thunk that some later reader forces on the request path.
              (newCache `seq` old {cache = newCache, cacheSize = cacheSize old + sizeOfRes}, ())
      registerKeyWithSidecar env cacheKey ttlInSeconds
      pure res

registerKeyWithSidecar :: MonadIO m => InMemEnv -> Text -> Seconds -> m ()
registerKeyWithSidecar env cacheKey cacheTtl =
  case inMemSidecarEnv env of
    Nothing -> pure ()
    Just sidecar -> liftIO $
      void $
        forkIO $ do
          let req = RegisterKeyRequest {keyName = cacheKey, keySchema = Nothing, ttlInSeconds = Just cacheTtl}
          void (callRegisterKey (sidecarManager sidecar) (sidecarBaseUrl sidecar) (inMemManagementToken env) req)
            `catch` (\(_ :: SomeException) -> pure ())

refreshInMem :: (MonadFlow m, MonadReader r m, HasInMemEnv r, HedisFlow m r, TryException m) => Text -> m ()
refreshInMem keyInfix = do
  inMemEnv <- asks (.inMemEnv)
  now <- getCurrentTime
  liftIO $
    atomicModifyIORef' (inMemHashMap inMemEnv) $ \old ->
      let toKeep = HM.filterWithKey (\k _ -> not (keyInfix `T.isInfixOf` k)) (cache old)
          newSize = foldl' (\acc infoa -> acc + infoa.cacheDataSize) 0 (HM.elems toKeep)
       in (toKeep `seq` InMemCacheInfo {cache = toKeep, cacheSize = newSize, createdAt = old.createdAt}, ())
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
      void (callRefresh (sidecarManager sidecar) (sidecarBaseUrl sidecar) (inMemManagementToken inMemEnv) req)
        `catch` (\(_ :: SomeException) -> pure ())
    _ -> pure ()

inMemCleanupThread :: Maybe HedisEnv -> InMemEnv -> IORef (Maybe ByteString) -> IO ()
inMemCleanupThread mbHedisEnv inMemEnv lastAppliedForceCleanup = do
  -- One Redis hiccup must not kill the sweeper for the rest of the pod's
  -- life — without this catch, a single connection error stops TTL cleanup
  -- permanently and the cache grows without any brake.
  inMemCleanupCycle mbHedisEnv inMemEnv lastAppliedForceCleanup
    `catch` \(e :: SomeException) -> putStrLn ("inMemCleanupThread: cycle failed: " <> show e :: String)
  threadDelaySec $ 60

-- | One cleanup cycle. Decisions that need the whole map (LRU ordering) are
-- computed on a snapshot, but removal is applied with an atomic modify of the
-- LIVE map — the previous read-compute-writeIORef sequence silently deleted
-- every insert that landed during the sweep window (which grew with cache
-- size, making pods age faster the bigger their cache got).
inMemCleanupCycle :: Maybe HedisEnv -> InMemEnv -> IORef (Maybe ByteString) -> IO ()
inMemCleanupCycle mbHedisEnv inMemEnv lastAppliedForceCleanup = do
  let inMemCache = inMemEnv.inMemHashMap
      maxInMemSize = inMemEnv.maxInMemSize
  -- Network I/O first, before any cache reads, so the snapshot->apply window
  -- stays as small as possible.
  mbForceCleanup <- readForceCleanupFlag
  now <- getCurrentTime
  snapshot <- readIORef inMemCache
  let isExpired keyInfo = addUTCTime (secondsToNominalDiffTime keyInfo.ttlInSeconds) keyInfo.createdAt < now
      liveEntries = HM.filter (not . isExpired) (cache snapshot)
      liveSize = foldl' (\acc keyInfo -> acc + keyInfo.cacheDataSize) 0 (HM.elems liveEntries)
      anyExpired = HM.size liveEntries < HM.size (cache snapshot)
      -- LRU eviction set, decided on the snapshot (needs a full sort — too
      -- heavy to run inside the atomic section).
      lruDropSet =
        if liveSize > maxInMemSize
          then
            let targetSize = floor (fromIntegral maxInMemSize * 0.75 :: Double) :: Bytes
                newestFirst = reverse $ sortOn (\(_, InMemKeyInfo {lastUsed}) -> lastUsed) $ HM.toList liveEntries
                (_, dropped) =
                  foldl'
                    ( \(acc, drops) (k, InMemKeyInfo {cacheDataSize}) ->
                        if cacheDataSize + acc <= targetSize
                          then (acc + cacheDataSize, drops)
                          else (acc, HS.insert k drops)
                    )
                    (0, HS.empty)
                    newestFirst
             in dropped
          else HS.empty
      matchesForce k = case mbForceCleanup of
        Just (Just cleanupKeyPrefix) -> cleanupKeyPrefix `T.isInfixOf` k
        Just Nothing -> True -- full wipe requested
        Nothing -> False
      -- TTL is re-checked against the entry actually in the live map, so a
      -- key recached during the sweep (fresh createdAt) is kept.
      shouldDrop k v = isExpired v || k `HS.member` lruDropSet || matchesForce k
  when (anyExpired || not (HS.null lruDropSet) || isJust mbForceCleanup) $
    atomicModifyIORef' inMemCache $ \cur ->
      let newCache = HM.filterWithKey (\k v -> not (shouldDrop k v)) (cache cur)
          newSize = foldl' (\acc keyInfo -> acc + keyInfo.cacheDataSize) 0 (HM.elems newCache)
       in (newCache `seq` InMemCacheInfo {cache = newCache, cacheSize = newSize, createdAt = cur.createdAt}, ())
  where
    -- Just (Just prefix) = clean keys matching prefix; Just Nothing = full
    -- wipe; Nothing = no (new) flag. Each flag value is applied once — its
    -- write time makes every refresh a new value; reading never extends TTL.
    readForceCleanupFlag :: IO (Maybe (Maybe Text))
    readForceCleanupFlag = case mbHedisEnv of
      Nothing -> pure Nothing
      Just hedisEnv -> do
        let key = cs $ hedisEnv.keyModifier "inmem:force:cleanup:timeofday"
        forceCleanupValue <- Hedis.runRedis hedisEnv.hedisConnection (Hedis.get key)
        case forceCleanupValue of
          Left err -> do
            print err
            pure Nothing
          Right forceCleanupVal -> do
            let forceCleanupExpiryValue :: Maybe ForceCleanupExpiryValue = Ae.decode . BSL.fromStrict =<< forceCleanupVal
            case forceCleanupExpiryValue of
              Nothing -> pure Nothing
              Just cacheExpiryValue -> do
                lastApplied <- readIORef lastAppliedForceCleanup
                if forceCleanupVal /= lastApplied
                  then do
                    writeIORef lastAppliedForceCleanup forceCleanupVal
                    pure (Just (forceCleanupKeyPrefix cacheExpiryValue))
                  else pure Nothing

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
      lastAppliedForceCleanup <- newIORef Nothing
      let inMemEnv =
            InMemEnv
              { enableInMem = inMemConfig.enableInMem,
                maxInMemSize = inMemConfig.maxInMemSize,
                inMemHashMap = inMemHashMap,
                inMemSidecarEnv = mbSidecarEnv,
                inMemManagementToken = mgmtToken,
                inMemServiceName = svcName
              }
      void $ forkIO $ forever $ inMemCleanupThread mbHedisEnv inMemEnv lastAppliedForceCleanup
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
