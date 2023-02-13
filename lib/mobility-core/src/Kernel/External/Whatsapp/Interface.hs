module Kernel.External.Whatsapp.Interface
  ( module Reexport,
    whatsAppOptApi,
    whatsAppOtpApi
  )
where

import Kernel.External.Whatsapp.GupShup.Config as Reexport
import qualified Kernel.External.Whatsapp.Interface.GupShup as GupShup
import Kernel.External.Whatsapp.Interface.Types as Reexport
import Kernel.External.Whatsapp.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import EulerHS.Prelude


whatsAppOptApi :: (EncFlow m r, EsqDBFlow m r, CoreMetrics m) => WhatsappHandler m -> OptApiReq -> m OptApiResp
whatsAppOptApi WhatsappHandler {..} req = do
  prividersPriorityList <- getProvidersPriorityList
  when (null prividersPriorityList) $ throwError $ InternalError "No whatsapp serive provider configured"
  callWithFallback prividersPriorityList
  where
    callWithFallback [] = throwError $ InternalError "Not able to opt whatsapp with all the configured providers"
    callWithFallback (preferredProvider : restProviders) = do
      whatsappConfig <- getProviderConfig preferredProvider
      result <- try @_ @SomeException $ whatsAppOptApi' whatsappConfig req
      case result of
        Left _ -> callWithFallback restProviders
        Right res -> pure res

whatsAppOptApi' ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  WhatsappServiceConfig ->
  OptApiReq ->
  m OptApiResp
whatsAppOptApi' serviceConfig req = case serviceConfig of
  GupShupConfig cfg -> GupShup.whatsAppOptApi cfg req


whatsAppOtpApi :: (EncFlow m r, EsqDBFlow m r, CoreMetrics m) => WhatsappHandler m -> SendOtpApiReq -> m SendOtpApiResp
whatsAppOtpApi WhatsappHandler {..} req = do
  prividersPriorityList <- getProvidersPriorityList
  when (null prividersPriorityList) $ throwError $ InternalError "No whatsapp serive provider configured"
  callWithFallback prividersPriorityList
  where
    callWithFallback [] = throwError $ InternalError "Not able to opt whatsapp with all the configured providers"
    callWithFallback (preferredProvider : restProviders) = do
      whatsappConfig <- getProviderConfig preferredProvider
      result <- try @_ @SomeException $ whatsAppOtpApi' whatsappConfig req
      case result of
        Left _ -> callWithFallback restProviders
        Right res -> pure res

whatsAppOtpApi' ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  WhatsappServiceConfig ->
  SendOtpApiReq ->
  m SendOtpApiResp
whatsAppOtpApi' serviceConfig req = case serviceConfig of
  GupShupConfig cfg -> GupShup.whatsAppOTPApi cfg req
