{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.Interface.GupShup
  ( module Reexport,
    sendOTP,
  )
where

import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.SMS.GupShup.Config
import qualified Kernel.External.SMS.GupShup.Flow as GF
import Kernel.External.SMS.Interface.Types as IT
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
  GupShupCfg ->
  IT.SendSMSReq ->
  m IT.SendSMSRes
sendOTP gupShupCfg SendSMSReq {..} = do
  let gupShupOtpSmsTemplate = smsBody
  let gupShupPhoneNumber = phoneNumber
  gupShupUserId <- decrypt gupShupCfg.userName
  gupShupPassword <- decrypt gupShupCfg.password
  gupShupTemplateId <- decrypt gupShupCfg.templateId
  gupShupEntityId <- decrypt gupShupCfg.entityId
  res <- GF.sendOTPApi gupShupOtpSmsTemplate gupShupPhoneNumber gupShupUserId gupShupPassword gupShupEntityId gupShupTemplateId sender gupShupCfg
  return $ returnSmsResultGupShup res.response.status

returnSmsResultGupShup :: Text -> IT.SendSMSRes
returnSmsResultGupShup txt = case txt of
  "success" -> Success
  _ -> Fail
