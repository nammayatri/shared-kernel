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

module Kernel.External.SOS.GJ112.Flow
  ( sendSOS,
    validateGJ112Response,
  )
where

import qualified EulerHS.Types as ET
import Kernel.External.Encryption
import Kernel.External.SOS.GJ112.API
import Kernel.External.SOS.GJ112.Auth
import Kernel.External.SOS.GJ112.Config
import Kernel.External.SOS.GJ112.Types
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common

-- | Validate GJ112 API response.
-- GJ112 may return HTTP 200 with non-200 responseCode in the body.
-- This function checks the responseCode and throws an appropriate GJ112Error
-- if the operation was not successful.
validateGJ112Response ::
  (MonadFlow m) =>
  GJ112SOSRes ->
  m GJ112SOSRes
validateGJ112Response res
  | isGJ112Success res = pure res
  | otherwise = do
    let errMsg = fromMaybe "Unknown error" res.message
    logDebug $ "GJ112 validation error: " <> errMsg
    throwM $ GJ112OperationFailure errMsg

-- | Send SOS Event to GJ112
sendSOS ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  GJ112Cfg ->
  GJ112SOSReq ->
  m GJ112SOSRes
sendSOS config req = do
  gj112Token <- getGJ112Token config
  let authToken = GJ112AuthToken gj112Token.token
  res <-
    callGJ112API
      config.baseUrl
      (ET.client gj112SOSAPI (Just authToken) req)
      "GJ112 SOS Event"
      gj112SOSAPI
  logDebug $ "GJ112 SOS response: " <> show res
  validateGJ112Response res

-- | Call GJ112 API with error handling (following ERSS/MMI pattern)
callGJ112API :: CallAPI m r api a
callGJ112API =
  callApiUnwrappingApiError
    (identity @GJ112Error)
    Nothing
    (Just "GJ112_API_ERROR")
    Nothing
