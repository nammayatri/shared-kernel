{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.ExotelSms.Flow where

import Data.Text as T
import EulerHS.Prelude
import EulerHS.Types as ET
import qualified Kernel.External.SMS.ExotelSms.API as API
import Kernel.External.SMS.ExotelSms.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common as B
import Servant hiding (throwError)
import Servant.Client

defaultBaseUrlSms :: ExotelSmsSID -> ExotelURL -> BaseUrl
defaultBaseUrlSms sid exoUrl =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = getExotelAccountURL exoUrl,
      baseUrlPort = 443,
      baseUrlPath =
        T.unpack $
          "/v1/Accounts/"
            <> getExotelSmsSID sid
            <> "/Sms/send.json"
    }

sendOTPApi ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  ExotelURL ->
  BasicAuthData ->
  ExotelSmsSID ->
  Text ->
  Text ->
  Text ->
  m
    SubmitSmsResp
sendOTPApi
  url
  authData
  sid
  otpSmsTemplate
  phoneNumber
  sender = do
    let submitSmsReq = SubmitSmsReq sender phoneNumber otpSmsTemplate
    let auth = authData
    callExotelAPI
      (defaultBaseUrlSms sid url)
      (callExotel auth submitSmsReq)
      "sendOTPApi"
      API.exotelConnectAPI
    where
      callExotel auth submitSmsReq = ET.client API.exotelConnectAPI auth submitSmsReq

callExotelAPI :: CallAPI env api a
callExotelAPI =
  callApiUnwrappingApiError
    (identity @ExotelError)
    Nothing
    (Just "EXOTEL_NOT_AVAILABLE")
    Nothing
