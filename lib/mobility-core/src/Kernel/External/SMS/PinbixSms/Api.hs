{-# LANGUAGE DataKinds #-}

module Kernel.External.SMS.PinbixSms.Api where

import EulerHS.Prelude
import Kernel.External.SMS.PinbixSms.Types
import Kernel.Types.App
import Servant

type ServiceAPI =
  MandatoryQueryParam "userid" Text
    :> MandatoryQueryParam "password" Text
    :> MandatoryQueryParam "sendMethod" Text
    :> MandatoryQueryParam "senderid" Text
    :> MandatoryQueryParam "msgType" Text
    :> MandatoryQueryParam "output" Text
    :> MandatoryQueryParam "mobile" Text
    :> MandatoryQueryParam "msg" Text
    :> Get '[JSON] PinbixSmsResponse

pinbixSmsSendAPI :: Proxy ServiceAPI
pinbixSmsSendAPI = Proxy
