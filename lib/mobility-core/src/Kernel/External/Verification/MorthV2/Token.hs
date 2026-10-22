{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

 distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

 FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

 General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.MorthV2.Token
  ( getCachedJwt,
    invalidateCachedJwt,
  )
where

import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.Verification.MorthV2.Flow as Flow
import qualified Kernel.External.Verification.MorthV2.Types as T
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common (MonadFlow, TryException)
import Kernel.Types.Error (MorthV2Error (..))
import Kernel.Utils.Error.Throwing (throwError)
import Kernel.Utils.Logging (logInfo)
import Kernel.Utils.Servant.Client (HasRequestId)

type JwtCacheM m r =
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m,
    Hedis.HedisFlow m r,
    TryException m
  )

getCachedJwt :: JwtCacheM m r => T.MorthV2VerificationCfg -> m Text
getCachedJwt cfg = do
  let key = jwtCacheKey cfg.clientId
      lockKey = key <> ":refresh-lock"
  cached <- Hedis.runInMasterCloudRedisCell $ Hedis.safeGet key
  case cached of
    Just t -> pure t
    Nothing -> do
      lockAcquired <- Hedis.runInMasterCloudRedisCell $ Hedis.tryLockRedis lockKey 30
      if lockAcquired
        then
          ( do
              tokenResp <- Flow.callToken cfg
              let ttlSec = max 60 (tokenResp.expiresInMs `div` 1000 - 60)
              Hedis.runInMasterCloudRedisCell $ Hedis.setExp key tokenResp.token ttlSec
              pure tokenResp.token
          )
            `finally` (Hedis.runInMasterCloudRedisCell $ Hedis.unlockRedis lockKey)
        else do
          logInfo "MorthV2 token refresh lock held by another pod; waiting 3s"
          threadDelay 3000000
          Hedis.runInMasterCloudRedisCell (Hedis.safeGet key) >>= \case
            Just t -> pure t
            Nothing -> throwError MorthV2TokenMissing

invalidateCachedJwt ::
  (Hedis.HedisFlow m r, TryException m) =>
  Text ->
  m ()
invalidateCachedJwt clientId =
  Hedis.runInMasterCloudRedisCell $ Hedis.del (jwtCacheKey clientId)

jwtCacheKey :: Text -> Text
jwtCacheKey clientId = "morth_v2:jwt:" <> clientId
