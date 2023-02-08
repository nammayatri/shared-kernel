module Kernel.External.SMS.ExotelSms.API where

import Kernel.External.SMS.ExotelSms.Types
import EulerHS.Prelude
import Servant hiding (throwError)

type ExotelConnectAPI =
  BasicAuth "apikey-apitoken" BasicAuthData
    :> ReqBody '[FormUrlEncoded] SubmitSmsReq
    :> Post '[JSON] SubmitSmsResp

exotelConnectAPI :: Proxy ExotelConnectAPI
exotelConnectAPI = Proxy