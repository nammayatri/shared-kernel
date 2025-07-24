{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.GupShup.Flow where

import Data.Text as T
import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.External.Encryption
import qualified Kernel.External.SMS.GupShup.API as API
import Kernel.External.SMS.GupShup.Config
import Kernel.External.SMS.GupShup.Types
  ( SubmitSmsRes,
  )
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common as B
import Servant.Client.Core (ClientError)

sendOTPApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r
  ) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  GupShupCfg ->
  m SubmitSmsRes
sendOTPApi
  otpSmsTemplate
  phoneNumber
  gupShupUserId
  gupShupPassword
  gupShupEntityId
  gupShupTemplateId
  gupShupSender
  gupShupSmsCfg = do
    logDebug $ "GupShup req :" <> otpSmsTemplate <> " phoneNumber " <> phoneNumber <> "gupShupSender" <> gupShupSender <> "gupShupSmsCfg : " <> show gupShupSmsCfg
    res <-
      callAPI
        gupShupSmsCfg.url
        (callGupShup "SendMessage" 1.1 gupShupUserId gupShupPassword phoneNumber otpSmsTemplate "TEXT" "plain" "json" gupShupEntityId gupShupTemplateId gupShupSender)
        "sendOTPApi"
        API.gupShupConnectAPI
        >>= checkGupShupOptError gupShupSmsCfg.url
    logDebug $ "GupShup response: " <> show res
    pure res
    where
      callGupShup = ET.client API.gupShupConnectAPI

checkGupShupOptError :: (MonadThrow m, B.Log m) => BaseUrl -> Either ClientError SubmitSmsRes -> m SubmitSmsRes
checkGupShupOptError url res =
  fromEitherM (gupShupOptError url) res >>= validateResponseStatus

validateResponseStatus :: (MonadThrow m, B.Log m) => SubmitSmsRes -> m SubmitSmsRes
validateResponseStatus response = do
  let resp = response.response
  case resp.status of
    "success" -> pure response
    _ -> do
      case resp.id of
        "100" -> throwError GupShupUnknownServerError
        "101" -> throwError GupShupInvalidRequest
        "102" -> throwError GupShupUnauthorized
        "103" -> throwError GupShupUserIdNotFound
        "105" -> throwError GupShupInvalidPhoneNumber
        "106" -> throwError GupShupWrongMethodService
        "175" -> throwError GupShupInterNationalPhoneNumber
        "322" -> throwError GupShupTooManyRequests
        _ -> throwError GupShupUnknownServerError

gupShupOptError :: BaseUrl -> ClientError -> ExternalAPICallError
gupShupOptError = ExternalAPICallError (Just "GUPSHUP_OPT_API_ERROR")
