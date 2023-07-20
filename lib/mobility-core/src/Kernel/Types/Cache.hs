{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Kernel.Types.Cache where

import Kernel.Prelude
import Kernel.Storage.Hedis.Config
import Kernel.Types.Time (Seconds)
import Kernel.Utils.Dhall (FromDhall)

newtype CacheConfig = CacheConfig
  { configsExpTime :: Seconds
  }
  deriving (Generic, FromDhall)

type HasCacheConfig r = HasField "cacheConfig" r CacheConfig

type CacheFlow m r = (HasCacheConfig r, HedisFlow m r)

class Cache a m where
  type CacheKey a
  getKey :: CacheKey a -> m (Maybe a)
  setKey :: CacheKey a -> a -> m ()
  delKey :: CacheKey a -> m ()

class Cache a m => CacheEx a m where
  setKeyEx :: Seconds -> CacheKey a -> a -> m ()

caching ::
  (CacheEx a m, Monad m) =>
  (a -> Seconds) ->
  (CacheKey a -> m (Maybe a)) ->
  CacheKey a ->
  m (Maybe a)
caching getTtl getData key =
  getKey key >>= \case
    Nothing -> do
      mbRes <- getData key
      whenJust mbRes \res -> setKeyEx (getTtl res) key res
      pure mbRes
    res -> pure res
