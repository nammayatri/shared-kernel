{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.Interface.DigoEngageSms
  ( module Reexport,
    sendOTP,
  )
where

import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.SMS.DigoEngage.Config
import qualified Kernel.External.SMS.DigoEngage.Flow as Flow
import Kernel.External.SMS.DigoEngage.Types (ClientDomain (..), SubmitSmsReq (..), SubmitSmsRes (..), TLVForSmsReq (..))
import Kernel.External.SMS.Interface.Types as IT
import Kernel.External.SMS.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m
  ) =>
  DigoEngageSmsCfg ->
  IT.SendSMSReq ->
  m IT.SendSMSRes
sendOTP cfg smsReq = do
  let digoReq =
        SubmitSmsReq
          { from = cfg.from,
            to = smsReq.phoneNumber,
            msg = smsReq.smsBody,
            dlr = ClientDomain {mask = cfg.dlr.mask, url = cfg.dlr.url},
            messageType = cfg.messageType,
            tiny = cfg.tiny,
            tlv = TLVForSmsReq {peId = cfg.tlv.peId, templateId = fromMaybe "" smsReq.templateId, telemarketerId = cfg.tlv.telemarketerId}
          }
  res <- Flow.sendOTPApi cfg digoReq
  return $ returnSmsResultDigoEngage res

returnSmsResultDigoEngage :: SubmitSmsRes -> IT.SendSMSRes
returnSmsResultDigoEngage (SmsSuccess _) = Success
returnSmsResultDigoEngage (SmsError _) = Fail
