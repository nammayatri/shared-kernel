{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Whatsapp.Interface.Cerf
  ( whatsAppOTPApi,
    whatsAppSendMessageWithTemplateIdAPI,
  )
where

import EulerHS.Prelude
import Kernel.External.Whatsapp.Cerf.Config
import qualified Kernel.External.Whatsapp.Cerf.Flow as Flow
import qualified Kernel.External.Whatsapp.Cerf.Types as CT
import Kernel.External.Whatsapp.Interface.Types as IT
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

-- | Sends the OTP through the configured otpCampaignName: OTP as the template param and as the
-- copy-code button value. Without it, throwing lets the caller fall back to the next provider
-- in the priority list.
whatsAppOTPApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  CerfCfg ->
  IT.SendOtpApiReq ->
  m IT.SendOtpApiResp
whatsAppOTPApi cerfCfg req = do
  otpCampaign <- cerfCfg.otpCampaignName & fromMaybeM (InternalError "otpCampaignName is not configured for CERF WhatsApp")
  res <- Flow.sendMessageApi cerfCfg req.sendTo otpCampaign [req.var1] (Just [CT.mkCopyCodeButton req.var1])
  pure $ toOtpApiResp req.sendTo res

-- | templateId is the CERF campaign name; variables are sent as templateParams in order.
whatsAppSendMessageWithTemplateIdAPI ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  CerfCfg ->
  IT.SendWhatsAppMessageWithTemplateIdApIReq ->
  m IT.SendOtpApiResp
whatsAppSendMessageWithTemplateIdAPI cerfCfg req = do
  res <- Flow.sendMessageApi cerfCfg req.sendTo req.templateId (catMaybes req.variables) Nothing
  pure $ toOtpApiResp req.sendTo res

toOtpApiResp :: Text -> CT.CerfWhatsappRes -> IT.SendOtpApiResp
toOtpApiResp phone res =
  let resp =
        IT.OptApiResponse
          { IT.id = fromMaybe "" res.submittedMessageId,
            IT.phone = phone,
            IT.details = fromMaybe "submitted" res.message,
            IT.status = if res.success then "success" else "failed"
          }
   in IT.OptApiResp
        { IT._response = resp,
          IT._data = Just $ IT.OptApiRespData [resp]
        }
