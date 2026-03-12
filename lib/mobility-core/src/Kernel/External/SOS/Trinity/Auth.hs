{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Kernel.External.SOS.Trinity.Auth
  ( getTrinityToken,
    TrinityToken (..),
    redisTrinityKey,
  )
where

import qualified Data.Time.Clock as Time
import qualified EulerHS.Types as ET
import Kernel.External.Encryption
import Kernel.External.SOS.Trinity.API
import Kernel.External.SOS.Trinity.Config
import Kernel.External.SOS.Trinity.Types
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common

-- | Cached token with expiry timestamp
data TrinityToken = TrinityToken
  { token :: Text,
    expiresAtUTC :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

redisTrinityKey :: Text
redisTrinityKey = "Core:trinity_token"

-- | Get valid Trinity token (check cache, validate, get new if needed)
getTrinityToken ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  TrinityCfg ->
  m TrinityToken
getTrinityToken config = do
  let redisKey = config.tokenKeyPrefix <> ":" <> redisTrinityKey
  cached <- Redis.get redisKey
  now <- liftIO Time.getCurrentTime
  case cached of
    Nothing -> getNewToken config
    Just cachedToken ->
      if now < cachedToken.expiresAtUTC
        then pure cachedToken
        else getNewToken config

-- | Get new token using authorizationKey + credentials
getNewToken ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  TrinityCfg ->
  m TrinityToken
getNewToken config = do
  authKeyDecrypted <- decrypt config.authorizationKey
  usernameDecrypted <- decrypt config.username
  passwordDecrypted <- decrypt config.password

  let req =
        TrinityAuthReq
          { authorizationKey = authKeyDecrypted,
            username = usernameDecrypted,
            password = passwordDecrypted,
            grantType = "password",
            scope = "default",
            apim = "subscribe"
          }

  res <-
    callTrinityAuthAPI
      config.baseUrl
      (ET.client trinityAuthAPI req)
      "trinityAuth"
      trinityAuthAPI

  now <- liftIO Time.getCurrentTime
  let expiresIn = fromMaybe 3600 res.expires_in
      bufferedExpiry = Time.addUTCTime (fromIntegral expiresIn - 60) now

  let trinityToken =
        TrinityToken
          { token = res.access_token,
            expiresAtUTC = bufferedExpiry
          }

  let redisKey = config.tokenKeyPrefix <> ":" <> redisTrinityKey
  Redis.set redisKey trinityToken
  pure trinityToken

callTrinityAuthAPI :: CallAPI m r api a
callTrinityAuthAPI =
  callApiUnwrappingApiError
    (identity @TrinityError)
    Nothing
    (Just "TRINITY_AUTH_ERROR")
    Nothing
