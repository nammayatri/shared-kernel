{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.Types.CacheFlow where

import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Common
import Kernel.Utils.Dhall

data CacheConfig = CacheConfig
  { configsExpTime :: Seconds,
    farePolicyExpTime :: Seconds
  }
  deriving (Generic, FromDhall)

data CacConfig = CacConfig
  { host :: String,
    interval :: Natural,
    tenant :: String,
    retryConnection :: Bool,
    cacExpTime :: Seconds,
    enablePolling :: Bool,
    enableCac :: Bool
  }
  deriving (Generic, FromDhall)

data SuperPositionConfig = SuperPositionConfig
  { host :: String,
    interval :: Natural,
    tenants :: [String],
    retryConnection :: Bool,
    enablePolling :: Bool,
    enableSuperPosition :: Bool
  }
  deriving (Generic, FromDhall)

type HasCacheConfig r = HasField "cacheConfig" r CacheConfig

type HasCacConfig r = HasField "cacConfig" r CacConfig

type CacheFlow m r = (HasCacheConfig r, HedisFlow m r, HasCacConfig r)
