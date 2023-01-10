module Beckn.External.SMS.ExotelSms.API where

import Beckn.External.SMS.ExotelSms.Types
import EulerHS.Prelude
import Servant hiding (throwError)

type ExotelConnectAPI =
  BasicAuth "apikey-apitoken" BasicAuthData
    :> ReqBody '[FormUrlEncoded] SubmitSmsReq
    :> Post '[JSON] SubmitSmsResp

exotelConnectAPI :: Proxy ExotelConnectAPI
exotelConnectAPI = Proxy