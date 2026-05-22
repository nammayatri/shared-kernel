module Kernel.External.SMS.KaleyraSms.API where

import EulerHS.Prelude
import Kernel.External.SMS.KaleyraSms.Types
import Servant hiding (throwError)

type KaleyraSmsAPI =
  "v1"
    :> Capture "sid" Text
    :> "messages"
    :> Header' '[Required, Strict] "api-key" Text
    :> ReqBody '[JSON] KaleyraSmsReq
    :> Post '[JSON] KaleyraSmsRes

kaleyraConnectAPI :: Proxy KaleyraSmsAPI
kaleyraConnectAPI = Proxy
