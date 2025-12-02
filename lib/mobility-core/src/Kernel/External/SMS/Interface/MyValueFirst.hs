{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.Interface.MyValueFirst
  ( module Reexport,
    sendOTP,
  )
where

import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.SMS.Interface.Types as IT
import Kernel.External.SMS.MyValueFirst.Config
import qualified Kernel.External.SMS.MyValueFirst.Flow as MVF
import Kernel.External.SMS.MyValueFirst.Types as MT
import Kernel.External.SMS.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Servant.Client

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m,
    HasRequestId r,
    MonadReader r m
  ) =>
  MyValueFirstCfg ->
  IT.SendSMSReq ->
  m IT.SendSMSRes
sendOTP smsCfg SendSMSReq {..} = do
  let urlAddress = smsCfg.url
      otpSmsTemp = smsBody
      phone = phoneNumber
      senderName = sender
  token <- decrypt smsCfg.token
  res <- MVF.sendOTPApi urlAddress token otpSmsTemp phone senderName

  return $ returnSmsResultMVF res

returnSmsResultMVF :: MT.SubmitSmsRes -> IT.SendSMSRes
returnSmsResultMVF txt =
  case txt of
    Sent -> Success
    BadNumber -> Fail
    _ -> IT.UnknownError
