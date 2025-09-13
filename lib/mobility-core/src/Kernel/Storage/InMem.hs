{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Kernel.Storage.InMem where

import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as BA
import qualified Data.HashMap.Strict as HM
import Data.String.Conversions
import Data.Typeable
import EulerHS.Prelude
import Kernel.Types.App (MonadFlow)
import Kernel.Types.CacheFlow
import Text.Hex (encodeHex)
import Unsafe.Coerce (unsafeCoerce)

withInMemCache :: forall a b r m. (Show a, Eq a, MonadFlow m, MonadReader r m, HasInMemConfig r, Typeable b) => [a] -> m b -> m b
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
      inMemConfig <- asks (.inMemConfig)
      let inMemCache = inMemHashMap inMemConfig
      mbRes <- HM.lookup key <$> readIORef inMemCache
      case mbRes of
        Just resAny -> pure (unsafeCoerce resAny)
        Nothing -> do
          res <- fn
          void $ atomicModifyIORef inMemCache (\old -> (HM.insert cacheKey (unsafeCoerce @_ @Any res) old, ()))
          pure res
