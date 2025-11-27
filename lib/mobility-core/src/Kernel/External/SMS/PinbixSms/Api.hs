{-# LANGUAGE DataKinds #-}

module Kernel.External.SMS.PinbixSms.Api where

import EulerHS.Prelude
import Kernel.External.SMS.PinbixSms.Types
import Servant

type ServiceAPI =
  QueryParam' '[Required] "userid" Text
    :> QueryParam' '[Required] "password" Text
    :> QueryParam' '[Required] "sendMethod" Text
    :> QueryParam' '[Required] "senderid" Text
    :> QueryParam' '[Required] "msgType" Text
    :> QueryParam' '[Required] "output" Text
    :> QueryParam' '[Required] "mobile" Text
    :> QueryParam' '[Required] "msg" Text
    :> Get '[JSON] PinbixSmsResponse

pinbixSmsSendAPI :: Proxy ServiceAPI
pinbixSmsSendAPI = Proxy
