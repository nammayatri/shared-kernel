module Kernel.External.Infobip.API.WebengageWebhook where

import Kernel.External.Infobip.Types
import Kernel.Types.APISuccess (APISuccess)
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant

type ServiceAPI =
  "tracking"
    :> "privatessp-events"
    :> ReqBody '[JSON] WebengageRes
    :> Post '[JSON] APISuccess

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

sendStatus :: WebengageRes -> ET.EulerClient APISuccess
sendStatus = ET.client serviceAPI
