{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.DigoEngage.Flow where

import qualified Data.Text.Encoding as DT
import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.External.Encryption (decrypt)
import Kernel.External.SMS.DigoEngage.API as API
import Kernel.External.SMS.DigoEngage.Config
import Kernel.External.SMS.DigoEngage.Types
import Kernel.External.SMS.Interface.Types as IT
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common as B
import Servant hiding (throwError)
import Servant.Client

sendOTPApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r
  ) =>
  DigoEngageSmsCfg ->
  IT.SendSMSReq ->
  m SubmitSmsRes
sendOTPApi digoEngageSmsCfg req = do
  apiKey_ <- decrypt digoEngageSmsCfg.apiKey
  password_ <- decrypt digoEngageSmsCfg.password
  let authData =
        BasicAuthData
          (DT.encodeUtf8 $ digoEngageSmsCfg.username)
          (DT.encodeUtf8 $ password_)
      authHeader = Just $ "Bearer " <> apiKey_
  callAPI
    digoEngageSmsCfg.url
    (callDigoEngage authHeader authData req)
    "sendOTPApi"
    API.digoConnectAPI
    >>= checkDigoEngageOptError digoEngageSmsCfg.url
  where
    callDigoEngage = ET.client API.digoConnectAPI

checkDigoEngageOptError :: (MonadThrow m, B.Log m) => BaseUrl -> Either ClientError SubmitSmsRes -> m SubmitSmsRes
checkDigoEngageOptError url res =
  fromEitherM (digoEngageOptError url) res >>= validateResponseStatus

validateResponseStatus :: (MonadThrow m, B.Log m) => SubmitSmsRes -> m SubmitSmsRes
validateResponseStatus (SmsSuccess res) = pure (SmsSuccess res)
validateResponseStatus (SmsError err) =
  case err.status of
    0 -> case err.errorDetails of
      Just details -> do
        logDebug $ "DigoEngage validation error: " <> show details
        throwError $ DigoEngageValidationError err.message
      Nothing -> throwError DigoEngageUnknownServerError
    _ -> throwError DigoEngageUnknownServerError

digoEngageOptError :: BaseUrl -> ClientError -> ExternalAPICallError
digoEngageOptError = ExternalAPICallError (Just "DIGOENGAGE_OPT_API_ERROR")
