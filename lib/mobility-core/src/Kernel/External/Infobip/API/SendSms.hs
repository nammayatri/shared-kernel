module Kernel.External.Infobip.API.SendSms where

import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.External.Infobip.Types
import Servant

type ServiceAPI =
  "sms" :> "1" :> "text" :> "query"
    :> QueryParam "username" Text
    :> QueryParam "password" Text
    :> QueryParam "from" Text
    :> QueryParam "to" Text
    :> QueryParam "text" Text
    :> QueryParam "indiaDltPrincipalEntityId" Text
    :> QueryParam "indiaDltContentTemplateId" Text
    :> QueryParam "notifyUrl" Text
    :> Post '[JSON] SMSRes

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

sendSms :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> ET.EulerClient SMSRes
sendSms username password from to text indiaDltPrincipalEntityId indiaDltContentTemplateId url = ET.client serviceAPI (Just username) (Just password) (Just from) (Just to) (Just text) (Just indiaDltContentTemplateId) (Just indiaDltPrincipalEntityId) (Just url)
