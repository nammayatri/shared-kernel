{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.Interface.VonageSms
  ( module Reexport,
    sendOTP,
  )
where

import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.SMS.Interface.Types as IT
import Kernel.External.SMS.Types as Reexport
import Kernel.External.SMS.VonageSms.Config
import qualified Kernel.External.SMS.VonageSms.Flow as Flow
import Kernel.External.SMS.VonageSms.Types (SubmitSmsRes (..), VonageSmsReq (..))
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
  VonageSmsCfg ->
  IT.SendSMSReq ->
  m IT.SendSMSRes
sendOTP cfg smsReq = do
  let vonageReq =
        VonageSmsReq
          { apiKey = "",
            apiSecret = "",
            from = if null smsReq.sender then cfg.from else smsReq.sender,
            to = smsReq.phoneNumber,
            text = smsReq.smsBody
          }
  res <- Flow.sendOTPApi cfg vonageReq
  return $ returnSmsResultVonage res

returnSmsResultVonage :: SubmitSmsRes -> IT.SendSMSRes
returnSmsResultVonage (SmsSuccess _) = Success
returnSmsResultVonage (SmsError _) = Fail
