module Kernel.External.Insurance.IffcoTokio.Config where

import qualified Data.HashMap.Internal as HashMap
import Kernel.Prelude
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as HttpTLS

prepareIffcoTokioHttpManager :: Int -> HashMap.HashMap Text Http.ManagerSettings
prepareIffcoTokioHttpManager timeout =
  HashMap.singleton (fromString iffcoTokioHttpManagerKey) $
    HttpTLS.tlsManagerSettings
      { Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)
      }

iffcoTokioHttpManagerKey :: String
iffcoTokioHttpManagerKey = "iffco-tokio-http-manager"
