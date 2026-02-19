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

module Kernel.External.SOS.ERSS.Auth
  ( getERSSToken,
    ERSSToken (..),
    redisERSSKey,
  )
where

import qualified Data.Time.Clock as Time
import qualified EulerHS.Types as ET
import Kernel.External.Encryption
import Kernel.External.SOS.ERSS.API
import Kernel.External.SOS.ERSS.Config
import Kernel.External.SOS.ERSS.Types
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common

-- | Cached token with expiry timestamps
data ERSSToken = ERSSToken
  { accessToken :: Text,
    refreshToken :: Text,
    accessExpiresAt :: UTCTime,
    refreshExpiresAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Redis key for ERSS token caching
redisERSSKey :: Text
redisERSSKey = "Core:erss_token"

-- | Get valid ERSS token (check cache, validate, refresh if needed)
getERSSToken ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  ERSSCfg ->
  m ERSSToken
getERSSToken config = do
  let redisKey = config.tokenKeyPrefix <> ":" <> redisERSSKey
  cached <- Redis.get redisKey
  now <- liftIO Time.getCurrentTime
  case cached of
    Nothing -> getNewPasswordToken config
    Just token ->
      if now < token.accessExpiresAt
        then pure token
        else
          if now < token.refreshExpiresAt
            then refreshAccessToken config token
            else getNewPasswordToken config

-- | Get new tokens using password grant
getNewPasswordToken ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  ERSSCfg ->
  m ERSSToken
getNewPasswordToken config = do
  clientSecretDecrypted <- decrypt config.clientSecret
  usernameDecrypted <- decrypt config.username
  passwordDecrypted <- decrypt config.password

  let req =
        ERSSPasswordGrantReq
          { grant_type = "password",
            client_id = config.clientId,
            client_secret = clientSecretDecrypted,
            username = usernameDecrypted,
            password = passwordDecrypted
          }

  res <-
    callERSSAuthAPI
      config.baseUrl
      (ET.client erssPasswordAuthAPI req)
      "ERSS Password Auth"
      erssPasswordAuthAPI

  now <- liftIO Time.getCurrentTime
  let token =
        ERSSToken
          { accessToken = res.access_token,
            refreshToken = res.refresh_token,
            accessExpiresAt = Time.addUTCTime (fromIntegral res.expires_in - 60) now,
            refreshExpiresAt = Time.addUTCTime (fromIntegral res.refresh_expires_in - 60) now
          }

  let redisKey = config.tokenKeyPrefix <> ":" <> redisERSSKey
  Redis.set redisKey token
  pure token

-- | Refresh access token using refresh_token grant
refreshAccessToken ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  ERSSCfg ->
  ERSSToken ->
  m ERSSToken
refreshAccessToken config oldToken = do
  clientSecretDecrypted <- decrypt config.clientSecret

  let req =
        ERSSRefreshGrantReq
          { grant_type = "refresh_token",
            client_id = config.clientId,
            client_secret = clientSecretDecrypted,
            refresh_token = oldToken.refreshToken
          }

  res <-
    callERSSAuthAPI
      config.baseUrl
      (ET.client erssRefreshAuthAPI req)
      "ERSS Refresh Auth"
      erssRefreshAuthAPI

  now <- liftIO Time.getCurrentTime
  let token =
        ERSSToken
          { accessToken = res.access_token,
            refreshToken = res.refresh_token,
            accessExpiresAt = Time.addUTCTime (fromIntegral res.expires_in - 60) now,
            refreshExpiresAt = Time.addUTCTime (fromIntegral res.refresh_expires_in - 60) now
          }

  let redisKey = config.tokenKeyPrefix <> ":" <> redisERSSKey
  Redis.set redisKey token
  pure token

-- | Call ERSS Auth API with error handling (following MMI pattern)
callERSSAuthAPI :: CallAPI m r api a
callERSSAuthAPI =
  callApiUnwrappingApiError
    (identity @ERSSError)
    Nothing
    (Just "ERSS_AUTH_ERROR")
    Nothing
