{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Whatsapp.Interface.Twilio
  ( module Reexport,
    whatsAppOTPApi,
    whatsAppSendMessageWithTemplateIdAPI,
  )
where

import Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.SMS.TwillioSms.Config
import qualified Kernel.External.SMS.TwillioSms.Flow as Ex
import qualified Kernel.External.SMS.TwillioSms.Types as SmsTypes
import Kernel.External.Whatsapp.Interface.Types as IT
import Kernel.External.Whatsapp.Types as Reexport
import Kernel.Prelude hiding (length, map)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common

whatsAppOTPApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m,
    HasRequestId r,
    MonadReader r m
  ) =>
  TwillioSmsCfg ->
  IT.SendOtpApiReq ->
  m IT.SendOtpApiResp
whatsAppOTPApi cfg SendOtpApiReq {..} = do
  messageServiceId <- decrypt cfg.messageServiceId
  let whatsappTo = "whatsapp:" <> sendTo
      twilioReq =
        SmsTypes.TwillioSmsReq
          { to = whatsappTo,
            body = var1,
            messagingServiceSid = messageServiceId,
            from = Nothing
          }
  res <- Ex.sendOTPApi cfg twilioReq
  return $
    OptApiResp
      { _response =
          OptApiResponse
            { id = res.sid,
              phone = sendTo,
              details = "WhatsApp OTP " <> twilioStatusToWhatsappStatus res.status <> " via Twilio",
              status = twilioStatusToWhatsappStatus res.status
            },
        _data = Nothing
      }

whatsAppSendMessageWithTemplateIdAPI ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m,
    HasRequestId r,
    MonadReader r m
  ) =>
  TwillioSmsCfg ->
  IT.SendWhatsAppMessageWithTemplateIdApIReq ->
  m IT.SendWhatsAppMessageApiResp
whatsAppSendMessageWithTemplateIdAPI cfg SendWhatsAppMessageWithTemplateIdApIReq {..} = do
  messageServiceId <- decrypt cfg.messageServiceId
  let whatsappTo = "whatsapp:" <> sendTo
  res <-
    if T.null templateId
      then do
        -- Freeform fallback: concatenate all variables as the message body
        let messageBody = T.intercalate "\n" $ mapMaybe identity variables
        Ex.sendOTPApi cfg $
          SmsTypes.TwillioSmsReq
            { to = whatsappTo,
              body = messageBody,
              messagingServiceSid = messageServiceId,
              from = Nothing
            }
      else do
        -- Template message: ContentSid + ContentVariables
        let varMap = Map.fromList $ zipWith (\i v -> (T.pack (show (i :: Int)), fromMaybe "" v)) [1 ..] variables
            contentVariablesJson = decodeUtf8 $ encode varMap
        Ex.sendWhatsAppTemplateApi cfg $
          SmsTypes.TwillioWhatsAppTemplateReq
            { to = whatsappTo,
              contentSid = templateId,
              contentVariables = contentVariablesJson,
              messagingServiceSid = messageServiceId
            }
  return $
    OptApiResp
      { _response =
          OptApiResponse
            { id = res.sid,
              phone = sendTo,
              details = "WhatsApp message " <> twilioStatusToWhatsappStatus res.status <> " via Twilio",
              status = twilioStatusToWhatsappStatus res.status
            },
        _data = Nothing
      }

-- Convert Twilio SMS status to WhatsApp-compatible status
twilioStatusToWhatsappStatus :: SmsTypes.TwillioSmsStatus -> Text
twilioStatusToWhatsappStatus status = case status of
  SmsTypes.ACCEPTED -> "success"
  SmsTypes.SENT -> "success"
  SmsTypes.DELIVERED -> "success"
  SmsTypes.RECEIVED -> "success"
  SmsTypes.READ -> "success"
  SmsTypes.QUEUED -> "pending"
  SmsTypes.SCHEDULED -> "pending"
  SmsTypes.SENDING -> "pending"
  SmsTypes.RECEIVING -> "pending"
  SmsTypes.UNDELIVERED -> "failed"
  SmsTypes.FAILED -> "failed"
  SmsTypes.CANCELED -> "failed"
