{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.VonageSms.Flow where

import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.External.Encryption (decrypt)
import Kernel.External.SMS.VonageSms.API
import Kernel.External.SMS.VonageSms.Config
import Kernel.External.SMS.VonageSms.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common as B
import Servant.Client

sendOTPApi ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r
  ) =>
  VonageSmsCfg ->
  VonageSmsReq ->
  m SubmitSmsRes
sendOTPApi vonageSmsCfg req = do
  apiKey_ <- decrypt vonageSmsCfg.apiKey
  apiSecret_ <- decrypt vonageSmsCfg.apiSecret
  let vonageReq =
        VonageSmsReq
          { apiKey = apiKey_,
            apiSecret = apiSecret_,
            from = req.from,
            to = req.to,
            text = req.text
          }
  let eulerClient = ET.client (Proxy @VonageSmsAPI)
  res <-
    callAPI
      vonageSmsCfg.url
      (eulerClient "json" vonageReq)
      "sendOTPApi"
      (Proxy @VonageSmsAPI)
      >>= checkVonageOptError vonageSmsCfg.url
  logDebug $ "Vonage response: " <> show res
  pure $ validateVonageResponse res

checkVonageOptError :: (MonadThrow m, B.Log m) => BaseUrl -> Either ClientError VonageSmsRes -> m VonageSmsRes
checkVonageOptError url res =
  fromEitherM (vonageOptError url) res

validateVonageResponse :: VonageSmsRes -> SubmitSmsRes
validateVonageResponse VonageSmsRes {messages = []} =
  SmsError $ VonageErrorResponse "0" []
validateVonageResponse res@VonageSmsRes {messages = (msg : _)} =
  case msg.status of
    "0" -> SmsSuccess res
    errorCode -> SmsError $ VonageErrorResponse "1" [VonageErrorMessage errorCode (fromMaybe "Unknown error" msg.errorText)]

vonageOptError :: BaseUrl -> ClientError -> ExternalAPICallError
vonageOptError = ExternalAPICallError (Just "VONAGE_SMS_API_ERROR")
