{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Kernel.External.SMS.Interface
  ( module Reexport,
    sendSMS,
    checkSmsResult,
  )
where

import EulerHS.Prelude
import Kernel.External.SMS.ExotelSms.Config as Reexport
import Kernel.External.SMS.GupShup.Config as Reexport
import qualified Kernel.External.SMS.Interface.ExotelSms as ExotelSms
import qualified Kernel.External.SMS.Interface.GupShup as GupShup
import qualified Kernel.External.SMS.Interface.MyValueFirst as MyValueFirst
import qualified Kernel.External.SMS.Interface.TwillioSms as TwillioSms
import Kernel.External.SMS.Interface.Types as Reexport
import Kernel.External.SMS.MyValueFirst.Config as Reexport
import Kernel.External.SMS.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

sendSMS :: (EncFlow m r, EsqDBFlow m r, CoreMetrics m) => SmsHandler m -> SendSMSReq -> m SendSMSRes
sendSMS SmsHandler {..} req = do
  prividersPriorityList <- getProvidersPriorityList
  when (null prividersPriorityList) $ throwError $ InternalError "No sms serive provider configured"
  sendSmsWithFallback prividersPriorityList
  where
    sendSmsWithFallback [] = throwError $ InternalError "Not able to send sms with all the configured providers"
    sendSmsWithFallback (preferredProvider : restProviders) = do
      smsConfig <- getProviderConfig preferredProvider
      result <- try @_ @SomeException $ sendSMS' smsConfig req
      case result of
        Left _ -> sendSmsWithFallback restProviders
        Right res -> case res of
          UnknownError -> sendSmsWithFallback restProviders
          Fail -> sendSmsWithFallback restProviders
          _ -> pure res

sendSMS' ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  SmsServiceConfig ->
  SendSMSReq ->
  m SendSMSRes
sendSMS' serviceConfig req = case serviceConfig of
  ExotelSmsConfig cfg -> ExotelSms.sendOTP cfg req
  MyValueFirstConfig cfg -> MyValueFirst.sendOTP cfg req
  GupShupConfig cfg -> GupShup.sendOTP cfg req
  TwillioSmsConfig cfg -> TwillioSms.sendOTP cfg req

checkSmsResult ::
  (Log m, MonadThrow m) => SendSMSRes -> m ()
checkSmsResult txt =
  case txt of
    Success -> pure ()
    Fail -> throwError SMSInvalidNumber
    Pending -> pure ()
    _ -> throwError SMSInvalidNumber
