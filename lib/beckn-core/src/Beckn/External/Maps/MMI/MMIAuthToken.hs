{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Beckn.External.Maps.MMI.MMIAuthToken where

import Beckn.External.Encryption
import Beckn.External.Maps.MMI.Config
import qualified Beckn.External.Maps.MMI.MapsClient.Types as MMI
import Beckn.Storage.Hedis as Redis
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Types.Error as ER
import Beckn.Utils.Common
import Data.Maybe
import EulerHS.Prelude
import qualified EulerHS.Types as ET
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
    MonadFlow m
  ) =>
  MMICfg ->
  m MMI.AuthResp
mmiAuthToken mmiCfg = do
  secretKey <- decrypt mmiCfg.mmiAuthSecret
  let url = mmiCfg.mmiAuthUrl
      clientId = mmiCfg.mmiAuthId
      grantType = "client_credentials"
      authReq = MMI.AuthRequest grantType clientId secretKey
  callMMIAPI
    url
    (callMMIAuth authReq)
    "mmi-auto-suggest"
  where
    callMMIAuth authReq = ET.client mmiAuthAPI authReq

callMMIAPI :: CallAPI env a
callMMIAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_AUTH_ERROR")

redisMMIKey :: Text
redisMMIKey = "beckn:mmi_token"

-- | Get MMI token
getMMIToken ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  MMICfg ->
  m AccessToken
getMMIToken config = do
  tokenStatus :: Maybe AccessToken <- Redis.get (config.mmiAuthId <> ":" <> redisMMIKey)
  case tokenStatus of
    Nothing -> refreshToken config
    Just token -> pure token

getTokenText ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  MMICfg ->
  m Text
getTokenText mfg = do
  token <- getMMIToken mfg
  pure $ mmiTokenType token <> " " <> mmiAccessToken token

refreshToken ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  MMICfg ->
  m AccessToken
refreshToken config = do
  res <- mmiAuthToken config
  let accessToken =
        AccessToken
          { mmiAccessToken = res.accessToken,
            mmiTokenType = res.tokenType
          }
  Redis.set (config.mmiAuthId <> ":" <> redisMMIKey) accessToken
  pure accessToken
