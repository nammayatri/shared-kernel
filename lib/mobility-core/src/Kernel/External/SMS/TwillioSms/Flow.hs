{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.TwillioSms.Flow where

import qualified Data.Text.Encoding as DT
import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.External.Encryption
import qualified Kernel.External.SMS.TwillioSms.API as API
import Kernel.External.SMS.TwillioSms.Config
import Kernel.External.SMS.TwillioSms.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common as B
import Servant hiding (throwError)
import Servant.Client.Core (ClientError)

sendOTPApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r
  ) =>
  TwillioSmsCfg ->
  TwillioSmsReq ->
  m TwillioSmsResp
sendOTPApi twillioConfigs req = do
  accountsid <- decrypt twillioConfigs.accountSid
  authtoken <- decrypt twillioConfigs.authToken
  let authData = BasicAuthData (DT.encodeUtf8 accountsid) (DT.encodeUtf8 authtoken)
  callAPI
    twillioConfigs.url
    (callTwillioSmsAPI accountsid authData req)
    "sendOTPApi"
    API.twillioSMSAPI
    >>= checkTwillioSmsError twillioConfigs.url
  where
    callTwillioSmsAPI = ET.client API.twillioSMSAPI

checkTwillioSmsError :: (MonadThrow m, B.Log m) => BaseUrl -> Either ClientError TwillioSmsResp -> m TwillioSmsResp
checkTwillioSmsError url res =
  fromEitherM (twillioError url) res >>= validateResponseStatus

validateResponseStatus :: (MonadThrow m, B.Log m) => TwillioSmsResp -> m TwillioSmsResp
validateResponseStatus resp = do
  case elem (resp.status) [FAILED, UNDELIVERED] of
    True -> do
      case resp.errorCode of
        Just "400" -> throwError TwillioBadRequest
        Just "403" -> throwError TwillioForbidden
        Just "404" -> throwError TwillioAPIDoesNotExist
        Just "410" -> throwError TwillioUnknownError
        Just "503" -> throwError TwillioInternalServerError
        Just "10001" -> throwError TwillioAccountNotActive
        Just "10002" -> throwError TwillioTrialAccountFound
        Just "10004" -> throwError TwillioConcurrencyLimitExceeded
        Just "11100" -> throwError TwillioInvalidURLFormat
        _ -> throwError TwillioUnknownError
    False -> return resp

twillioError :: BaseUrl -> ClientError -> ExternalAPICallError
twillioError = ExternalAPICallError (Just "TWILLIO_OPT_API_ERROR")
