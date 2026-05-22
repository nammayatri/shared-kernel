{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.Interface.KaleyraSms
  ( module Reexport,
    sendOTP,
  )
where

import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.SMS.Interface.Types as IT
import Kernel.External.SMS.KaleyraSms.Config
import qualified Kernel.External.SMS.KaleyraSms.Flow as Flow
import Kernel.External.SMS.KaleyraSms.Types (KaleyraSmsReq (..), KaleyraSmsRes (..))
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
  KaleyraSmsCfg ->
  IT.SendSMSReq ->
  m IT.SendSMSRes
sendOTP cfg smsReq = do
  let kaleyraReq =
        KaleyraSmsReq
          { to = smsReq.phoneNumber,
            messageType = fromMaybe "TXN" smsReq.messageType,
            sender = smsReq.sender,
            body = smsReq.smsBody
          }
  res <- Flow.sendOTPApi cfg kaleyraReq
  return $ returnSmsResultKaleyra res

returnSmsResultKaleyra :: KaleyraSmsRes -> IT.SendSMSRes
returnSmsResultKaleyra (KaleyraSmsSuccess _) = Success
returnSmsResultKaleyra (KaleyraSmsError _) = Fail
