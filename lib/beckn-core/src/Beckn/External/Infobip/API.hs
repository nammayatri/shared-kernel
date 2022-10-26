module Beckn.External.Infobip.API where

import Beckn.External.Infobip.Types
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant

type ServiceAPI =
  "sms" :> "2" :> "text" :> "advanced"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] SMSReq
    :> Post '[JSON] SMSRes

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

sendSms :: Text -> SMSReq -> ET.EulerClient SMSRes
sendSms auth req = ET.client serviceAPI (Just auth) req