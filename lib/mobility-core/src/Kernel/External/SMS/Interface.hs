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

import Data.Char (digitToInt)
import Data.List (length, (!!))
import qualified Data.Text as T
import EulerHS.Prelude hiding (length)
import Kernel.External.SMS.DigoEngage.Config as Reexport
import Kernel.External.SMS.ExotelSms.Config as Reexport
import Kernel.External.SMS.GupShup.Config as Reexport
import qualified Kernel.External.SMS.Interface.DigoEngageSms as DigoEngageSms
import qualified Kernel.External.SMS.Interface.ExotelSms as ExotelSms
import qualified Kernel.External.SMS.Interface.GupShup as GupShup
import qualified Kernel.External.SMS.Interface.KarixSms as KarixSms
import qualified Kernel.External.SMS.Interface.MyValueFirst as MyValueFirst
import qualified Kernel.External.SMS.Interface.PinbixSms as PinbixSms
import qualified Kernel.External.SMS.Interface.TwillioSms as TwillioSms
import Kernel.External.SMS.Interface.Types as Reexport
import qualified Kernel.External.SMS.Interface.VonageSms as VonageSms
import Kernel.External.SMS.KarixSms.Config as Reexport
import Kernel.External.SMS.MyValueFirst.Config as Reexport
import Kernel.External.SMS.PinbixSms.Config as Reexport
import Kernel.External.SMS.Types as Reexport
import Kernel.External.SMS.VonageSms.Config as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

sendSMS' ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  SmsServiceConfig ->
  SendSMSReq ->
  m SendSMSRes
sendSMS' serviceConfig req = do
  case serviceConfig of
    ExotelSmsConfig cfg -> ExotelSms.sendOTP cfg req
    MyValueFirstConfig cfg -> MyValueFirst.sendOTP cfg req
    GupShupConfig cfg -> GupShup.sendOTP cfg req
    TwillioSmsConfig cfg -> TwillioSms.sendOTP cfg req
    DigoEngageSmsConfig cfg -> DigoEngageSms.sendOTP cfg req
    VonageSmsConfig cfg -> VonageSms.sendOTP cfg req
    KarixSmsConfig cfg -> KarixSms.sendOTP cfg req
    PinbixSmsConfig cfg -> PinbixSms.sendOTP cfg req

checkSmsResult ::
  (Log m, MonadThrow m) => SendSMSRes -> m ()
checkSmsResult txt =
  case txt of
    Success -> pure ()
    Fail -> throwError SMSInvalidNumber
    Pending -> pure ()
    _ -> throwError SMSInvalidNumber

sendSMS ::
  (EncFlow m r, EsqDBFlow m r, CoreMetrics m) =>
  SmsHandler m ->
  SendSMSReq ->
  m SendSMSRes
sendSMS SmsHandler {..} req = do
  providers <- getProvidersPriorityList
  when (null providers) $
    throwError $
      InternalError "No sms serive provider configured"
  let totalCount = length providers
      phoneNum = req.phoneNumber
      lastDigit =
        if T.null phoneNum
          then 0
          else digitToInt (T.last phoneNum)

      startIndex = lastDigit `mod` totalCount
  circularAttempt providers totalCount startIndex 0
  where
    circularAttempt providers totalCount startIndex currentAttempts = do
      when (currentAttempts >= totalCount) $
        throwError $
          InternalError "Not able to send sms with all the configured providers"

      let currentIndex = (startIndex + currentAttempts) `mod` totalCount
          preferredProvider = providers !! currentIndex
      smsConfig <- getProviderConfig preferredProvider
      result <- try @_ @SomeException $ sendSMS' smsConfig req

      case result of
        Left _ -> fallback
        Right res -> case res of
          UnknownError -> fallback
          Fail -> fallback
          _ -> pure res
      where
        fallback = circularAttempt providers totalCount startIndex (currentAttempts + 1)
