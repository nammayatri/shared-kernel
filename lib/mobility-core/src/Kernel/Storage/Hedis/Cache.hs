{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.Hedis.Cache
  ( withRedisCache,
    delRedisCache,
    delRedisCacheBucket,
    buildRedisHashKey,
    buildRedisCacheField,
  )
where

import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as BA
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Storage.Hedis.Config (HedisFlow)
import Kernel.Storage.Hedis.Queries (ExpirationTime, del, hDel, hSetExp, safeHGet)
import Text.Hex (encodeHex)

-- | The Redis bucket is just the prefix; the cached type is no longer part of
-- the key, so deletes by bucket are type-free, e.g.
-- @buildRedisHashKey "ConfigPilot" == "ConfigPilot"@.
buildRedisHashKey :: Text -> Text
buildRedisHashKey prefix = prefix

buildRedisCacheField :: [Text] -> Maybe Text
buildRedisCacheField fieldFrags =
  let field = T.intercalate (":" :: Text) fieldFrags
   in if
          | T.length field > 200 ->
            Just . encodeHex . BA.convert @(Hash.Digest Hash.SHA256) $ Hash.hashlazy (cs field)
          | T.null field -> Nothing
          | otherwise -> Just field

withRedisCache ::
  forall b m env.
  (ToJSON b, FromJSON b, HedisFlow m env) =>
  Text ->
  [Text] ->
  ExpirationTime ->
  m b ->
  m b
withRedisCache hashPrefix fieldFrags ttlInSeconds fn =
  if ttlInSeconds > 0
    then case buildRedisCacheField fieldFrags of
      Nothing -> fn
      Just field -> do
        let hashKey = buildRedisHashKey hashPrefix
        mbRes <- safeHGet hashKey field
        case mbRes of
          Just res -> pure res
          Nothing -> do
            res <- fn
            hSetExp hashKey field res ttlInSeconds
            pure res
    else fn

-- | Delete a single cached entry created by 'withRedisCache', e.g.
-- @delRedisCache "ConfigPilot" ["blr", "auto", "x"]@.
delRedisCache ::
  HedisFlow m env =>
  Text ->
  [Text] ->
  m ()
delRedisCache hashPrefix fieldFrags =
  case buildRedisCacheField fieldFrags of
    Nothing -> pure ()
    Just field -> hDel (buildRedisHashKey hashPrefix) [field]

delRedisCacheBucket ::
  HedisFlow m env =>
  Text ->
  m ()
delRedisCacheBucket hashPrefix = del (buildRedisHashKey hashPrefix)
