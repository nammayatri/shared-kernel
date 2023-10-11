{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Whatsapp.Interface.GupShup
  ( module Reexport,
    whatsAppOptApi,
    whatsAppOTPApi,
    whatsAppSendMessageWithTemplateIdAPI,
  )
where

import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.Whatsapp.GupShup.Config
import qualified Kernel.External.Whatsapp.GupShup.Flow as Ex
import Kernel.External.Whatsapp.Interface.Types as IT
import Kernel.External.Whatsapp.Types as Reexport
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

whatsAppOptApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m
  ) =>
  GupShupCfg ->
  IT.OptApiReq ->
  m IT.OptApiResp
whatsAppOptApi GupShupCfg {..} OptApiReq {..} = do
  userId <- decrypt userid
  password' <- decrypt password
  gupShupUrl <- parseBaseUrl url
  Ex.whatsAppOptAPI gupShupUrl userId password' phoneNumber (show method) authScheme v channel format

whatsAppOTPApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m
  ) =>
  GupShupCfg ->
  IT.SendOtpApiReq ->
  m IT.SendOtpApiResp
whatsAppOTPApi GupShupCfg {..} SendOtpApiReq {..} = do
  userId <- decrypt userid
  password' <- decrypt password
  gupShupUrl <- parseBaseUrl url
  Ex.whatsAppSendOtpAPI gupShupUrl userId password' sendTo otpCfg.method authScheme v otpCfg.msgType format var1 otpCfg.templateId

whatsAppSendMessageWithTemplateIdAPI ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m
  ) =>
  GupShupCfg ->
  IT.SendWhatsAppMessageWithTemplateIdApIReq ->
  m IT.SendWhatsAppMessageApiResp
whatsAppSendMessageWithTemplateIdAPI GupShupCfg {..} SendWhatsAppMessageWithTemplateIdApIReq {..} = do
  userId <- decrypt userid
  password' <- decrypt password
  gupShupUrl <- parseBaseUrl url
  Ex.whatsAppSendMessageWithTemplateIdAPI gupShupUrl userId password' sendTo otpCfg.method authScheme v otpCfg.msgType format var1 var2 var3 containsUrlButton templateId
