{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

 distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

 FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

 General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Redis-backed JWT cache for the MoRTH v2.1 Parivahan service.
--
-- The @/api/auth/token@ response is valid for 5 minutes. We cache per
-- @clientId@ with a 60-second safety margin so we never hand out a token
-- that expires between our @GET@ and the Parivahan server's receipt.
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

-- | Return a valid JWT for the given client, fetching + caching if necessary.
getCachedJwt :: JwtCacheM m r => T.MorthV2VerificationCfg -> m Text
getCachedJwt cfg = do
  let key = jwtCacheKey cfg.clientId
  Hedis.safeGet key >>= \case
    Just t -> pure t
    Nothing -> do
      tokenResp <- Flow.callToken cfg
      -- Cache for (expiresInMs - 60s), floored at 60s for sanity.
      let ttlSec = max 60 (tokenResp.expiresInMs `div` 1000 - 60)
      Hedis.setExp key tokenResp.token ttlSec
      pure tokenResp.token

-- | Drop the cached JWT for this client. Call this after the server rejects
-- a token mid-flight (e.g. @MorthV2TokenExpired@/@MorthV2TokenInvalid@) so
-- the next call fetches a fresh one.
invalidateCachedJwt ::
  (Hedis.HedisFlow m r, TryException m) =>
  Text ->
  m ()
invalidateCachedJwt clientId = Hedis.del (jwtCacheKey clientId)

jwtCacheKey :: Text -> Text
jwtCacheKey clientId = "morth_v2:jwt:" <> clientId
