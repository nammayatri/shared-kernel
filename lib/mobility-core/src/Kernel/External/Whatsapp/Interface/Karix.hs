{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Whatsapp.Interface.Karix
  ( whatsAppOTPApi,
    whatsAppSendMessageWithTemplateIdAPI,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.External.Whatsapp.Interface.Types as IT
import Kernel.External.Whatsapp.Karix.Config
import Kernel.External.Whatsapp.Karix.Flow as Flow
import qualified Kernel.External.Whatsapp.Karix.Types as KC
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Servant.Client

listToIndexedMap :: [Text] -> Map.Map Text Text
listToIndexedMap xs =
  Map.fromList $
    zip (map (T.pack . show) [0 :: Int ..]) xs

whatsAppApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  KarixCfg ->
  Text ->
  [Text] ->
  Text ->
  m SendOtpApiResp
whatsAppApi karixCfg sendTo variables templateId = do
  let convertReq =
        KC.KarixWhatsAppMessageReq
          { message =
              KC.KarixMessage
                { channel = "WABA",
                  content =
                    KC.KarixContent
                      { preview_url = Just False,
                        type_ = "TEMPLATE",
                        template =
                          Just
                            KC.KarixTemplate
                              { templateId = templateId,
                                parameterValues = listToIndexedMap variables
                              }
                      },
                  recipient =
                    KC.KarixRecipient
                      { to = sendTo,
                        recipient_type = "individual",
                        reference = Nothing
                      },
                  sender = KC.KarixSender {from = karixCfg.from},
                  preferences = maybe Nothing (\webHookId -> Just $ KC.KarixPreferences {webHookDNId = Just webHookId}) karixCfg.webHookDNId
                },
            metaData = Just $ KC.KarixMetaData {version = karixCfg.version}
          }

  res <- Flow.sendMessageApi karixCfg convertReq
  return $ toOtpApiResp sendTo res

whatsAppOTPApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  KarixCfg ->
  IT.SendOtpApiReq ->
  m SendOtpApiResp
whatsAppOTPApi karixCfg req = whatsAppApi karixCfg req.sendTo [req.var1] karixCfg.otpTemplate

whatsAppSendMessageWithTemplateIdAPI ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  KarixCfg ->
  IT.SendWhatsAppMessageWithTemplateIdApIReq ->
  m SendOtpApiResp
whatsAppSendMessageWithTemplateIdAPI karixCfg req = whatsAppApi karixCfg req.sendTo (catMaybes req.variables) req.templateId

toOtpApiResp :: Text -> KC.KarixWhatsappSubmitRes -> IT.SendOtpApiResp
toOtpApiResp phone (KC.KarixWhatsappSuccess (KC.KarixWhatsAppResponse (Just wid) _ _)) =
  let resp =
        IT.OptApiResponse
          { IT.id = wid,
            IT.phone = phone,
            IT.details = "Success",
            IT.status = "sent"
          }
   in IT.OptApiResp
        { IT._response = resp,
          IT._data = Just $ IT.OptApiRespData [resp]
        }
toOtpApiResp phone (KC.KarixWhatsappSuccess (KC.KarixWhatsAppResponse Nothing _ _)) =
  let resp =
        IT.OptApiResponse
          { IT.id = "",
            IT.phone = phone,
            IT.details = "Success",
            IT.status = "sent"
          }
   in IT.OptApiResp
        { IT._response = resp,
          IT._data = Just $ IT.OptApiRespData [resp]
        }
toOtpApiResp phone (KC.KarixWhatsAppError (KC.KarixWhatsAppErrorResponse (KC.KarixWhatsAppErrorDetails _code msg _typ))) =
  let resp =
        IT.OptApiResponse
          { IT.id = "",
            IT.phone = phone,
            IT.details = msg,
            IT.status = "failed"
          }
   in IT.OptApiResp
        { IT._response = resp,
          IT._data = Nothing
        }
