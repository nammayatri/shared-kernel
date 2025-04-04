{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Whatsapp.Interface.TataCommunications
  ( whatsAppOTPApi,
    whatsAppSendMessageWithTemplateIdAPI,
  )
where

import EulerHS.Prelude
import Kernel.External.Whatsapp.Interface.Types as IT
import Kernel.External.Whatsapp.TataCommunications.Config (TataCommunicationsCfg (..))
import Kernel.External.Whatsapp.TataCommunications.Flow as Flow
import qualified Kernel.External.Whatsapp.TataCommunications.Types as TC
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

whatsAppSendMessageWithTemplateIdAPI ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r
  ) =>
  TataCommunicationsCfg ->
  IT.SendWhatsAppMessageWithTemplateIdApIReq ->
  m SendOtpApiResp
whatsAppSendMessageWithTemplateIdAPI tataCommunicationsCfg req = do
  let convertReq =
        TC.WhatsAppMessageReq
          { recipient = req.sendTo,
            from = tataCommunicationsCfg.from,
            type_ = tataCommunicationsCfg.type_,
            template =
              TC.Template
                { name = tataCommunicationsCfg.template.name,
                  language = TC.Language {code = tataCommunicationsCfg.template.language.code},
                  components =
                    [ TC.Component
                        { type_ = "body",
                          parameters = map (\v -> TC.Parameter {type_ = "text", text = v}) req.variables
                        }
                    ]
                }
          }

  res <- Flow.sendMessageApi tataCommunicationsCfg convertReq
  return $ toOptApiResp req.sendTo res

whatsAppOTPApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r
  ) =>
  TataCommunicationsCfg ->
  IT.SendOtpApiReq ->
  m SendOtpApiResp
whatsAppOTPApi tataCommunicationsCfg req = do
  let convertReq =
        TC.WhatsAppMessageReq
          { recipient = req.sendTo,
            from = tataCommunicationsCfg.from,
            type_ = tataCommunicationsCfg.type_,
            template =
              TC.Template
                { name = tataCommunicationsCfg.template.name,
                  language = TC.Language {code = tataCommunicationsCfg.template.language.code},
                  components =
                    [ TC.Component
                        { type_ = "body",
                          parameters = [TC.Parameter {type_ = "text", text = Just req.var1}]
                        }
                    ]
                }
          }

  res <- Flow.sendMessageApi tataCommunicationsCfg convertReq
  return $ toOptApiResp req.sendTo res

toOptApiResp :: Text -> TC.WhatsappSubmitRes -> IT.SendOtpApiResp
toOptApiResp phone (TC.WhatsappSuccess (TC.WhatsAppResponse wid)) =
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
toOptApiResp phone (TC.WhatsAppError (TC.WhatsAppErrorResponse (TC.WhatsAppErrorDetails _code msg typ))) =
  let resp =
        IT.OptApiResponse
          { IT.id = "",
            IT.phone = phone,
            IT.details = msg,
            IT.status = typ
          }
   in IT.OptApiResp
        { IT._response = resp,
          IT._data = Nothing
        }
