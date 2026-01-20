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
  )
where

import EulerHS.Prelude
import Kernel.External.Whatsapp.Interface.Types as IT
import Kernel.External.Whatsapp.Karix.Config
import Kernel.External.Whatsapp.Karix.Flow as Flow
import qualified Kernel.External.Whatsapp.Karix.Types as KC
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Logging
import Kernel.Utils.Servant.Client

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
whatsAppOTPApi karixCfg req = do
  logDebug $ "KarixCfg: " <> show karixCfg
  logDebug $ "SendOtpApiReq: " <> show req
  let convertReq =
        KC.KarixWhatsAppMessageReq
          { message =
              KC.KarixMessage
                { channel = "WABA",
                  content =
                    KC.KarixContent
                      { preview_url = Just True,
                        type_ = "MEDIA_TEMPLATE",
                        mediaTemplate =
                          Just
                            KC.KarixTemplate
                              { autoTemplate = "*" <> req.var1 <> "*" <> karixCfg.otpTemplate,
                                buttons = Nothing
                              },
                        shorten_url = Just False
                      },
                  recipient =
                    KC.KarixRecipient
                      { to = req.sendTo,
                        recipient_type = "individual",
                        reference = Nothing
                      },
                  sender = KC.KarixSender {from = karixCfg.from},
                  preferences = maybe Nothing (\webHookId -> Just $ KC.KarixPreferences {webHookDNId = Just webHookId}) karixCfg.webHookDNId
                },
            metaData = Just $ KC.KarixMetaData {version = karixCfg.version}
          }

  res <- Flow.sendMessageApi karixCfg convertReq
  return $ toOptApiResp req.sendTo res

toOptApiResp :: Text -> KC.KarixWhatsappSubmitRes -> IT.SendOtpApiResp
toOptApiResp phone (KC.KarixWhatsappSuccess (KC.KarixWhatsAppResponse (Just wid) _ _)) =
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
toOptApiResp phone (KC.KarixWhatsappSuccess (KC.KarixWhatsAppResponse Nothing _ _)) =
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
toOptApiResp phone (KC.KarixWhatsAppError (KC.KarixWhatsAppErrorResponse (KC.KarixWhatsAppErrorDetails _code msg _typ))) =
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
