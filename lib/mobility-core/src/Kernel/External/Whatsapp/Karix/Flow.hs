{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Whatsapp.Karix.Flow where

import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.External.Encryption (decrypt)
import Kernel.External.Whatsapp.Karix.Api
import Kernel.External.Whatsapp.Karix.Config
import Kernel.External.Whatsapp.Karix.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common as B
import Servant.Client

sendMessageApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  KarixCfg ->
  KarixWhatsAppMessageReq ->
  m KarixWhatsappSubmitRes
sendMessageApi karixCfg req = do
  apiToken_ <- decrypt karixCfg.apiToken
  let authHeader = Just $ "Bearer " <> apiToken_
  let eulerClient = ET.client (Proxy @KarixWhatsappAPI)
  res <-
    callAPI
      karixCfg.url
      (eulerClient authHeader req)
      "sendMessageApi"
      (Proxy @KarixWhatsappAPI)
      >>= checkKarixWhatsappError karixCfg.url
  logDebug $ "KarixWhatsappAPI response: " <> show res
  return res

checkKarixWhatsappError :: (MonadThrow m, B.Log m) => BaseUrl -> Either ClientError KarixWhatsappSubmitRes -> m KarixWhatsappSubmitRes
checkKarixWhatsappError url res =
  fromEitherM (karixWhatsappError url) res >>= validateResponseStatus

validateResponseStatus :: (MonadThrow m, B.Log m) => KarixWhatsappSubmitRes -> m KarixWhatsappSubmitRes
validateResponseStatus (KarixWhatsappSuccess res) = pure (KarixWhatsappSuccess res)
validateResponseStatus (KarixWhatsAppError errResponse) =
  let err = errResponse.error
   in case err.code of
        Just 400 -> do
          logDebug $ "Karix WhatsApp API Validation Error: " <> err.message
          throwError KarixWhatsappInvalidRequest
        Just 401 -> do
          logDebug $ "Karix WhatsApp API Authentication Error: " <> err.message
          throwError KarixWhatsappUnauthorized
        Just 403 -> do
          logDebug $ "Karix WhatsApp API Permission Denied: " <> err.message
          throwError KarixWhatsappPermissionDenied
        Just 404 -> do
          logDebug $ "Karix WhatsApp API User Not Found: " <> err.message
          throwError KarixWhatsappUserIdNotFound
        Just 429 -> do
          logDebug $ "Karix WhatsApp API Too Many Requests: " <> err.message
          throwError KarixWhatsappTooManyRequests
        Just 500 -> do
          logDebug $ "Karix WhatsApp API Internal Server Error: " <> err.message
          throwError KarixWhatsappUnknownServerError
        Just 503 -> do
          logDebug $ "Karix WhatsApp API Temporary downtime or overload: " <> err.message
          throwError KarixWhatsappServiceUnavailable
        _ -> do
          logDebug $ "Karix WhatsApp API Unknown Error: " <> err.message
          throwError $ KarixWhatsappGenericError err.message

karixWhatsappError :: BaseUrl -> ClientError -> ExternalAPICallError
karixWhatsappError = ExternalAPICallError (Just "KarixWhatsapp_API_Error")
