{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.Interface.WBSms
  ( module Reexport,
    sendOTP,
  )
where

import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.SMS.Interface.Types as IT
import Kernel.External.SMS.Types as Reexport
import Kernel.External.SMS.WBSms.Config
import qualified Kernel.External.SMS.WBSms.Flow as WBF
import Kernel.External.SMS.WBSms.Types as WT
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

data TempIDType = TemplSendOtp | TemplAltNo | TemplWelcome
  deriving (Show)

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    Log m
  ) =>
  WBSmsCfg ->
  IT.SendSMSReq ->
  m IT.SendSMSRes
sendOTP wbCfg SendSMSReq {..} = do
  let templateIdType = parseTemplIdType smsBody
  templateId <- case templateIdType of
    TemplSendOtp -> pure wbCfg.templSendOtp
    TemplAltNo -> pure wbCfg.templAltNo
    TemplWelcome -> pure wbCfg.templWelcome
  passkey <- decrypt wbCfg.passkey
  let wbUrl = wbCfg.url
      wbSmsTemplate = smsBody
      wbPhoneNumber = T.replace "+91" "" phoneNumber
  res <- WBF.sendOTPApi wbUrl passkey templateId wbSmsTemplate wbPhoneNumber
  return $ returnSmsResultWBF res

parseTemplIdType :: Text -> TempIDType
parseTemplIdType txt
  | T.isInfixOf "login to Yatri Sathi App" txt = TemplSendOtp
  | T.isInfixOf "adding alternate number in Yatri Sathi App" txt = TemplAltNo
  | otherwise = TemplWelcome

returnSmsResultWBF :: WT.SubmitSmsRes -> IT.SendSMSRes
returnSmsResultWBF _ = Success
