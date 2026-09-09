{-# LANGUAGE DataKinds #-}

module Kernel.External.SMS.CerfSms.API where

import EulerHS.Prelude
import Kernel.External.SMS.CerfSms.Types
import Kernel.Types.App
import Servant

-- | @GET \/pushapi\/sendmsg@
type PushServiceAPI =
  "pushapi"
    :> "sendmsg"
    :> MandatoryQueryParam "username" Text
    :> MandatoryQueryParam "dest" Text
    :> MandatoryQueryParam "apikey" Text
    :> MandatoryQueryParam "signature" Text
    :> MandatoryQueryParam "msgtype" Text
    :> MandatoryQueryParam "msgtxt" Text
    :> QueryParam "custref" Text
    :> QueryParam "campaign" Text
    :> Get '[JSON] CerfSmsResponse

cerfSmsPushAPI :: Proxy PushServiceAPI
cerfSmsPushAPI = Proxy

-- | @POST \/pushapi\/json\/sendbulkmsg@
type JsonServiceAPI =
  "pushapi"
    :> "json"
    :> "sendbulkmsg"
    :> ReqBody '[JSON] CerfSmsJsonReq
    :> Post '[JSON] CerfSmsJsonRes

cerfSmsJsonAPI :: Proxy JsonServiceAPI
cerfSmsJsonAPI = Proxy
