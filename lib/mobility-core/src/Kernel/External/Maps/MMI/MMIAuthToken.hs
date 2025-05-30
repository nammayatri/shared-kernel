{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Kernel.External.Maps.MMI.MMIAuthToken where

import Data.Maybe
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.External.Encryption
import Kernel.External.Maps.MMI.Config
import qualified Kernel.External.Maps.MMI.Types as MMI
import Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error as ER
import Kernel.Utils.Common
import qualified Kernel.Utils.ExternalAPICallLogging as ApiCallLogger
import qualified Kernel.Utils.Text as KUT
import Servant hiding (throwError)

type MMIAuthAPI =
  ReqBody '[FormUrlEncoded] MMI.AuthRequest
    :> Post '[JSON] MMI.AuthResp

data AccessToken = AccessToken
  { mmiAccessToken :: !Text,
    mmiTokenType :: !Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

mmiAuthAPI :: Proxy MMIAuthAPI
mmiAuthAPI = Proxy

mmiAuthToken ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MMICfg ->
  m MMI.AuthResp
mmiAuthToken entityId mmiCfg = do
  secretKey <- decrypt mmiCfg.mmiAuthSecret
  let url = mmiCfg.mmiAuthUrl
      clientId = mmiCfg.mmiAuthId
      grantType = "client_credentials"
      authReq = MMI.AuthRequest grantType clientId secretKey
  rsp <-
    callMMIAPI
      url
      (callMMIAuth authReq)
      "mmi-auto-suggest"
      mmiAuthAPI
  fork ("Logging external API Call of mmiAuthToken MMI ") $
    ApiCallLogger.pushExternalApiCallDataToKafkaWithTextEncodedResp "mmiAuthToken" "MMI" entityId (Nothing @(Maybe Value)) $ KUT.encodeToText rsp
  return rsp
  where
    callMMIAuth authReq = ET.client mmiAuthAPI authReq

callMMIAPI :: CallAPI env api a
callMMIAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_AUTH_ERROR")
    Nothing

redisMMIKey :: Text
redisMMIKey = "Core:mmi_token"

-- | Get MMI token
getMMIToken ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MMICfg ->
  m AccessToken
getMMIToken entityId config = do
  tokenStatus :: Maybe AccessToken <- Redis.get (config.mmiAuthId <> ":" <> redisMMIKey)
  case tokenStatus of
    Nothing -> refreshToken entityId config
    Just token -> pure token

getTokenText ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MMICfg ->
  m Text
getTokenText entityId mfg = do
  token <- getMMIToken entityId mfg
  pure $ mmiTokenType token <> " " <> mmiAccessToken token

refreshToken ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  MMICfg ->
  m AccessToken
refreshToken entityId config = do
  res <- mmiAuthToken entityId config
  let accessToken =
        AccessToken
          { mmiAccessToken = res.accessToken,
            mmiTokenType = res.tokenType
          }
  Redis.set (config.mmiAuthId <> ":" <> redisMMIKey) accessToken
  pure accessToken
