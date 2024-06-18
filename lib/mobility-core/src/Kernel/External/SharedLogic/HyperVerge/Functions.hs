module Kernel.External.SharedLogic.HyperVerge.Functions where

import qualified Data.HashMap.Internal as HMap
import qualified Data.Text as DT
import Kernel.Prelude
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http

prepareHyperVergeHttpManager :: Int -> HMap.HashMap DT.Text Http.ManagerSettings
prepareHyperVergeHttpManager timeout =
  HMap.singleton (DT.pack hyperVergeHttpManagerKey) $
    Http.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

hyperVergeHttpManagerKey :: String
hyperVergeHttpManagerKey = "hyperverge-http-manager"
