module Kernel.External.SMS.ExotelSms.API where

import EulerHS.Prelude
import Kernel.External.SMS.ExotelSms.Types
import Servant hiding (throwError)

type ExotelConnectAPI =
  BasicAuth "apikey-apitoken" BasicAuthData
    :> ReqBody '[FormUrlEncoded] SubmitSmsReq
    :> Post '[JSON] SubmitSmsResp

exotelConnectAPI :: Proxy ExotelConnectAPI
exotelConnectAPI = Proxy
