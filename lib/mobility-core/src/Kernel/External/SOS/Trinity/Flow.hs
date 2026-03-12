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

module Kernel.External.SOS.Trinity.Flow
  ( sendSOS,
    validateTrinityResponse,
  )
where

import qualified EulerHS.Types as ET
import Kernel.External.Encryption
import Kernel.External.SOS.Trinity.API
import Kernel.External.SOS.Trinity.Auth
import Kernel.External.SOS.Trinity.Config
import Kernel.External.SOS.Trinity.Types
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common

-- | Validate Trinity API response.
-- Trinity may return HTTP 200 with status=false in the body.
validateTrinityResponse ::
  (MonadFlow m) =>
  TrinitySOSRes ->
  m TrinitySOSRes
validateTrinityResponse res
  | isTrinitySuccess res = pure res
  | otherwise = do
    let errMsg = fromMaybe "Unknown error" res.message
    throwM $ TrinityOperationFailure errMsg

-- | Send SOS Event to Trinity
sendSOS ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  TrinityCfg ->
  TrinitySOSReq ->
  m TrinitySOSRes
sendSOS config req = do
  trinityToken <- getTrinityToken config
  let authToken = TrinityAuthToken trinityToken.token
  res <-
    callTrinityAPI
      config.sosUrl
      (ET.client trinitySOSAPI (Just authToken) req)
      "trinitySOSTrigger"
      trinitySOSAPI
  validateTrinityResponse res

callTrinityAPI :: CallAPI m r api a
callTrinityAPI =
  callApiUnwrappingApiError
    (identity @TrinityError)
    Nothing
    (Just "TRINITY_API_ERROR")
    Nothing
