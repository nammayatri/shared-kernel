{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Whatsapp.Cerf.Flow where

import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.External.Encryption (decrypt)
import Kernel.External.Whatsapp.Cerf.Api
import Kernel.External.Whatsapp.Cerf.Config
import qualified Kernel.External.Whatsapp.Cerf.Types as CT
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common as B

sendMessageApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  CerfCfg ->
  Text ->
  Text ->
  [Text] ->
  Maybe [CT.CerfButton] ->
  m CT.CerfWhatsappRes
sendMessageApi cerfCfg sendTo campaign params mbButtons = do
  decryptedKey <- decrypt cerfCfg.apiKey
  let req =
        CT.CerfWhatsappReq
          { CT.apiKey = decryptedKey,
            CT.campaignName = campaign,
            CT.destination = sendTo,
            CT.userName = cerfCfg.userName,
            CT.templateParams = params,
            CT.buttons = mbButtons
          }
      eulerClient = ET.client (Proxy @CerfWhatsappAPI)
  res <-
    callAPI cerfCfg.url (eulerClient req) "cerfSendMessageApi" (Proxy @CerfWhatsappAPI)
      >>= fromEitherM (ExternalAPICallError (Just "CERF_WHATSAPP_API_ERROR") cerfCfg.url)
  logDebug $ "CerfWhatsappAPI response: " <> show res
  unless res.success $
    throwError $ InternalError ("CERF WhatsApp send failed for campaign " <> campaign <> ": " <> fromMaybe "unknown error" res.message)
  pure res
