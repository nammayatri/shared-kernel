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
import qualified Data.ByteArray as BA
import qualified Data.HashMap.Strict as HM
import Data.String.Conversions
import Data.Typeable
import EulerHS.Prelude
import Kernel.Types.App (MonadFlow)
import Kernel.Types.CacheFlow
import Kernel.Utils.Time (getCurrentTime, threadDelaySec)
import Text.Hex (encodeHex)
import Unsafe.Coerce (unsafeCoerce)

withInMemCache :: forall a b r m. (Show a, Show b, Eq a, MonadFlow m, MonadReader r m, HasInMemEnv r, Typeable b) => [a] -> m b -> m b
withInMemCache cacheKeys fn = do
  let key = cs . intercalate ":" $ map show cacheKeys
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
      inMemEnv <- asks (.inMemEnv)
      let inMemCache = inMemHashMap inMemEnv
      mbRes <- HM.lookup cacheKey . cache <$> readIORef inMemCache
      case mbRes of
        Just resAny -> pure (unsafeCoerce (cachedData resAny))
        Nothing -> do
          res <- fn
          time <- getCurrentTime
          let sizeOfRes = sizeOf res
          void $ atomicModifyIORef inMemCache (\old -> (old {cache = HM.insert cacheKey (InMemKeyInfo {lastUsed = time, cachedData = unsafeCoerce @_ @Any res, cacheDataSize = sizeOfRes}) (cache old), cacheSize = (cacheSize old) + sizeOfRes}, ()))
          pure res
  where
    sizeOf :: Show b => b -> Bytes
    sizeOf a = fromIntegral . length $ (show :: b -> Text) a

inMemCleanupThread :: InMemEnv -> IO ()
inMemCleanupThread inMemEnv = do
  let inMemCache = inMemEnv.inMemHashMap
      maxInMemSize = inMemEnv.maxInMemSize
  inMemCacheInfo <- readIORef inMemCache
  let inMemCacheSize = cacheSize inMemCacheInfo
  when (inMemCacheSize > maxInMemSize) $ do
    let cacheList = reverse $ sortOn (\(_, InMemKeyInfo {lastUsed}) -> lastUsed) $ HM.toList $ cache inMemCacheInfo
        (updatedCacheSize, updatedCache) = foldl' (\(acc, accCacheList) (k, v@(InMemKeyInfo {cacheDataSize})) -> if cacheDataSize + acc <= (floor $ (fromIntegral maxInMemSize * 0.75 :: Double)) then (acc + cacheDataSize, accCacheList ++ [(k, v)]) else (acc, accCacheList)) (0, []) cacheList
        updatedCacheInfo = InMemCacheInfo {cache = HM.fromList updatedCache, cacheSize = updatedCacheSize}
    writeIORef inMemCache updatedCacheInfo
  threadDelaySec $ 300

setupInMemEnv :: InMemConfig -> IO InMemEnv
setupInMemEnv inMemConfig = do
  let inMemCacheInfo = InMemCacheInfo {cache = mempty, cacheSize = 0}
  inMemHashMap <- newIORef inMemCacheInfo
  let inMemEnv =
        InMemEnv
          { enableInMem = inMemConfig.enableInMem,
            maxInMemSize = inMemConfig.maxInMemSize,
            inMemHashMap = inMemHashMap
          }
  void $ forkIO $ forever $ inMemCleanupThread inMemEnv
  pure inMemEnv
