{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Whatsapp.TataCommunications.Flow where

import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.External.Encryption (decrypt)
import Kernel.External.Whatsapp.TataCommunications.Config
import Kernel.External.Whatsapp.TataCommunications.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common as B
import Servant hiding (throwError)
import Servant.Client

type TataCommunicationsWhatsappAPI =
  "v1" :> "whatsapp"
    :> "messages"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] WhatsAppMessageReq
    :> Post '[JSON] WhatsappSubmitRes

sendMessageApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r
  ) =>
  TataCommunicationsCfg ->
  WhatsAppMessageReq ->
  m WhatsappSubmitRes
sendMessageApi tataCommunicationsCfg req = do
  apiKey_ <- decrypt tataCommunicationsCfg.apiToken
  let authHeader = Just apiKey_
  let eulerClient = ET.client (Proxy @TataCommunicationsWhatsappAPI)
  res <-
    callAPI
      tataCommunicationsCfg.url
      (eulerClient authHeader req)
      "sendMessageApi"
      (Proxy @TataCommunicationsWhatsappAPI)
      >>= checkTataCommunicationsWhatsappError tataCommunicationsCfg.url
  logDebug $ "TataCommunicationsWhatsappAPI response: " <> show res
  return res

checkTataCommunicationsWhatsappError :: (MonadThrow m, B.Log m) => BaseUrl -> Either ClientError WhatsappSubmitRes -> m WhatsappSubmitRes
checkTataCommunicationsWhatsappError url res =
  fromEitherM (tataCommunicationsWhatsappError url) res >>= validateResponseStatus

validateResponseStatus :: (MonadThrow m, B.Log m) => WhatsappSubmitRes -> m WhatsappSubmitRes
validateResponseStatus (WhatsappSuccess res) = pure (WhatsappSuccess res)
validateResponseStatus (WhatsAppError errResponse) =
  let err = errDetails errResponse
   in case err.code of
        400 -> do
          logDebug $ "Tata WhatsApp API Validation Error: " <> err.message
          throwError TataCommunicationsWhatsappInvalidRequest
        401 -> do
          logDebug $ "Tata WhatsApp API Authentication Error: " <> err.message
          throwError TataCommunicationsWhatsappUnauthorized
        403 -> do
          logDebug $ "Tata WhatsApp API Permission Denied: " <> err.message
          throwError TataCommunicationsWhatsapPermissionDenied
        404 -> do
          logDebug $ "Tata WhatsApp API User Not Found: " <> err.message
          throwError TataCommunicationsWhatsappUserIdNotFound
        429 -> do
          logDebug $ "Tata WhatsApp API Too Many Requests: " <> err.message
          throwError TataCommunicationsWhatsappTooManyRequests
        500 -> do
          logDebug $ "Tata WhatsApp API Internal Server Error: " <> err.message
          throwError TataCommunicationsWhatsappUnknownServerError
        503 -> do
          logDebug $ "Tata WhatsApp API Temporary downtime or overload: " <> err.message
          throwError TataCommunicationsWhatsapServiceUnavailable
        _ -> do
          logDebug $ "Tata WhatsApp API Unknown Error: " <> err.message
          throwError $ TataCommunicationsWhatsappGenericError err.message

tataCommunicationsWhatsappError :: BaseUrl -> ClientError -> ExternalAPICallError
tataCommunicationsWhatsappError = ExternalAPICallError (Just "TataCommunicationsWhatsapp_API_Error")
