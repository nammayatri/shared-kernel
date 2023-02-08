module Kernel.Types.Registry.Routes where

import qualified Kernel.Types.Registry.API as API
import EulerHS.Prelude
import Servant

type LookupAPI =
  "lookup"
    :> ReqBody '[JSON] API.LookupRequest
    :> Post '[JSON] API.LookupResponse

lookupAPI :: Proxy LookupAPI
lookupAPI = Proxy
