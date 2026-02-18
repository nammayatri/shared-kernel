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

module Kernel.External.SOS.ERSS.Flow
  ( sendInitialSOS,
    sendSOSTrace,
    uploadMedia,
    validateERSSResponse,
  )
where

import qualified EulerHS.Language as L
import qualified EulerHS.Types as ET
import Kernel.External.Encryption
import Kernel.External.SOS.ERSS.API
import Kernel.External.SOS.ERSS.Auth
import Kernel.External.SOS.ERSS.Config
import Kernel.External.SOS.ERSS.Types
import Kernel.Prelude
import Kernel.ServantMultipart
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common

-- | Validate ERSS API response envelope.
-- C-DAC APIs may return HTTP 200 with resultCode "OPERATION_FAILURE" in the body.
-- This function checks the resultCode and throws an appropriate ERSSError
-- if the operation was not successful, ensuring consistent error handling
-- regardless of whether the failure comes as an HTTP error or API-level error.
validateERSSResponse ::
  (MonadFlow m) =>
  Text ->
  ERSSApiResponse a ->
  m (ERSSApiResponse a)
validateERSSResponse _apiName res
  | isERSSSuccess res = pure res
  | otherwise = do
    let errMsg = fromMaybe "Unknown error" res.errorMsg
    logDebug $ "ERSS validation error: " <> errMsg
    throwM $ ERSSOperationFailure errMsg

-- | Send Initial SOS Signal to C-DAC ERSS
sendInitialSOS ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  ERSSCfg ->
  ERSSInitialSOSReq ->
  m ERSSInitialSOSRes
sendInitialSOS config req = do
  token <- getERSSToken config
  let authToken = ERSSAuthToken token.accessToken
  res <-
    callERSSAPI
      config.baseUrl
      (ET.client erssInitialSOSAPI (Just authToken) req)
      "ERSS Initial SOS"
      erssInitialSOSAPI
  logDebug $ "ERSS Initial SOS response: " <> show res
  validateERSSResponse "Initial SOS" res

-- | Send SOS Trace (location update) to C-DAC ERSS
sendSOSTrace ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  ERSSCfg ->
  ERSSTraceReq ->
  m ERSSTraceRes
sendSOSTrace config req = do
  token <- getERSSToken config
  let authToken = ERSSAuthToken token.accessToken
  res <-
    callERSSAPI
      config.baseUrl
      (ET.client erssTraceAPI (Just authToken) req)
      "ERSS SOS Trace"
      erssTraceAPI
  logDebug $ "ERSS SOS Trace response: " <> show res
  validateERSSResponse "SOS Trace" res

-- | Upload Media File to ERSS
uploadMedia ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  ERSSCfg ->
  Text ->
  Text ->
  FilePath ->
  m ERSSMediaUploadRes
uploadMedia config phoneNumber fileName filePath = do
  token <- getERSSToken config
  let authToken = ERSSAuthToken token.accessToken
  boundary <- L.runIO genBoundary
  let multipartData =
        MultipartData
          []
          [ FileData "file" fileName "" filePath
          ]
  let apiCall =
        ET.client
          erssMediaUploadAPI
          config.authCode
          phoneNumber
          fileName
          (Just authToken)
          (boundary, multipartData)
  res <-
    callERSSAPI
      config.baseUrl
      apiCall
      "ERSS Media Upload"
      erssMediaUploadAPI
  logDebug $ "ERSS Media Upload response: " <> show res
  validateERSSResponse "Media Upload" res

-- | Call ERSS API with error handling (following MMI pattern)
callERSSAPI :: CallAPI m r api a
callERSSAPI =
  callApiUnwrappingApiError
    (identity @ERSSError)
    Nothing
    (Just "ERSS_API_ERROR")
    Nothing
