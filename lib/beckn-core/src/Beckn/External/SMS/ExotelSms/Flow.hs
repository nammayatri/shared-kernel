module Beckn.External.SMS.ExotelSms.Flow where

import qualified Beckn.External.SMS.ExotelSms.API as API
import Beckn.External.SMS.ExotelSms.Types
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common as B
import Data.Text as T
import EulerHS.Prelude
import EulerHS.Types as ET
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
    where
      callExotel auth submitSmsReq = ET.client API.exotelConnectAPI auth submitSmsReq

callExotelAPI :: CallAPI env a
callExotelAPI =
  callApiUnwrappingApiError
    (identity @ExotelError)
    Nothing
    (Just "EXOTEL_NOT_AVAILABLE")