module Kernel.External.SMS.Karix.Api where

import EulerHS.Prelude
import Kernel.External.SMS.Karix.Types
import Servant

-- Karix JSON API: POST /httpapi/JsonReceiver (JSON body)
type ServiceAPI =
  "httpapi"
    :> "JsonReceiver"
    :> ReqBody '[JSON] KarixRequest
    :> Post '[JSON] KarixSubmitRes

karixConnectAPI :: Proxy ServiceAPI
karixConnectAPI = Proxy
