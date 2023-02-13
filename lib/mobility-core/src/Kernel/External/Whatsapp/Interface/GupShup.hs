module Kernel.External.Whatsapp.Interface.GupShup
  ( module Reexport,
    whatsAppOptApi,
    whatsAppOTPApi
  )
where

import Kernel.External.Encryption
import qualified Kernel.External.Whatsapp.GupShup.Flow as Ex
import Kernel.External.Whatsapp.GupShup.Config
import Kernel.External.Whatsapp.Interface.Types as IT
import Kernel.External.Whatsapp.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import EulerHS.Prelude
import Kernel.Prelude

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
