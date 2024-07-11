{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.Interface.TwillioSms
  ( module Reexport,
    sendOTP,
  )
where

import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.SMS.Interface.Types as IT
import Kernel.External.SMS.TwillioSms.Config
import qualified Kernel.External.SMS.TwillioSms.Flow as Ex
import Kernel.External.SMS.TwillioSms.Types
import Kernel.External.SMS.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m
  ) =>
  TwillioSmsCfg ->
  IT.SendSMSReq ->
  m IT.SendSMSRes
sendOTP cfg SendSMSReq {..} = do
  messageServiceId <- decrypt cfg.messageServiceId
  let twillioReq = TwillioSmsReq {to = phoneNumber, body = smsBody, messagingServiceSid = messageServiceId}
  res <- Ex.sendOTPApi cfg twillioReq
  return $ returnSmsResult res.status

returnSmsResult :: TwillioSmsStatus -> IT.SendSMSRes
returnSmsResult status =
  case status of
    ACCEPTED -> Success
    SENT -> Success
    DELIVERED -> Success
    RECEIVED -> Success
    READ -> Success
    QUEUED -> Pending
    SCHEDULED -> Pending
    SENDING -> Pending
    RECEIVING -> Pending
    UNDELIVERED -> Fail
    FAILED -> Fail
    CANCELED -> Fail
