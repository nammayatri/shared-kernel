{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

module Kernel.External.SOS.GJ112.Auth
  ( getGJ112Token,
    GJ112Token (..),
    redisGJ112Key,
  )
where

import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as POSIX
import qualified EulerHS.Types as ET
import Kernel.External.Encryption
import Kernel.External.SOS.GJ112.API
import Kernel.External.SOS.GJ112.Config
import Kernel.External.SOS.GJ112.Types
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common

-- | Cached token with expiry timestamp
data GJ112Token = GJ112Token
  { token :: Text,
    expiresAtUTC :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

redisGJ112Key :: Text
redisGJ112Key = "Core:gj112_token"

-- | Get valid GJ112 token (check cache, validate, get new if needed)
getGJ112Token ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  GJ112Cfg ->
  m GJ112Token
getGJ112Token config = do
  let redisKey = config.tokenKeyPrefix <> ":" <> redisGJ112Key
  cached <- Redis.get redisKey
  now <- liftIO Time.getCurrentTime
  case cached of
    Nothing -> do
      getNewToken config
    Just cachedToken -> do
      if now < cachedToken.expiresAtUTC
        then pure cachedToken
        else do
          getNewToken config

-- | Get new token using Base64-encoded credentials
getNewToken ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  GJ112Cfg ->
  m GJ112Token
getNewToken config = do
  userNameDecrypted <- decrypt config.userName
  passwordDecrypted <- decrypt config.password

  let userNameB64 = TE.decodeUtf8 $ B64.encode $ TE.encodeUtf8 userNameDecrypted
      passwordB64 = TE.decodeUtf8 $ B64.encode $ TE.encodeUtf8 passwordDecrypted

  let req =
        GJ112AuthReq
          { userName = userNameB64,
            password = passwordB64
          }

  res <-
    callGJ112AuthAPI
      config.baseUrl
      (ET.client gj112AuthAPI req)
      "gj112Auth"
      gj112AuthAPI

  let expiryEpochMs = res.tokenCreationTime + (fromIntegral res.expiresAt * 1000)
      expiryUTC = POSIX.posixSecondsToUTCTime (fromIntegral expiryEpochMs / 1000)
  now <- liftIO Time.getCurrentTime
  let bufferedExpiry = min expiryUTC (Time.addUTCTime (fromIntegral res.expiresAt - 60) now)

  let gj112Token =
        GJ112Token
          { token = res.token,
            expiresAtUTC = bufferedExpiry
          }

  let redisKey = config.tokenKeyPrefix <> ":" <> redisGJ112Key
  Redis.set redisKey gj112Token

  pure gj112Token

callGJ112AuthAPI :: CallAPI m r api a
callGJ112AuthAPI =
  callApiUnwrappingApiError
    (identity @GJ112Error)
    Nothing
    (Just "GJ112_AUTH_ERROR")
    Nothing
