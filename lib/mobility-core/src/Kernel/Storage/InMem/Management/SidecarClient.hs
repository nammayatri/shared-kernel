module Kernel.Storage.InMem.Management.SidecarClient where

import Kernel.Prelude
import Kernel.Storage.InMem.Management.Types (RegisterKeyRequest (..), RegisterKeyResponse (..), SidecarRefreshRequest (..), SidecarRefreshResponse (..))
import qualified Network.HTTP.Client as HTTP
import Servant
import Servant.Client

type RegisterKeyAPI =
  "api"
    :> "registerKey"
    :> ReqBody '[JSON] RegisterKeyRequest
    :> Post '[JSON] RegisterKeyResponse

type RefreshAPI =
  "api"
    :> "refresh"
    :> ReqBody '[JSON] SidecarRefreshRequest
    :> Post '[JSON] SidecarRefreshResponse

registerKeyClient :: RegisterKeyRequest -> ClientM RegisterKeyResponse
registerKeyClient = client (Proxy :: Proxy RegisterKeyAPI)

refreshClient :: SidecarRefreshRequest -> ClientM SidecarRefreshResponse
refreshClient = client (Proxy :: Proxy RefreshAPI)

callRegisterKey :: HTTP.Manager -> BaseUrl -> RegisterKeyRequest -> IO (Either ClientError RegisterKeyResponse)
callRegisterKey manager baseUrl req =
  runClientM (registerKeyClient req) (mkClientEnv manager baseUrl)

callRefresh :: HTTP.Manager -> BaseUrl -> SidecarRefreshRequest -> IO (Either ClientError SidecarRefreshResponse)
callRefresh manager baseUrl req =
  runClientM (refreshClient req) (mkClientEnv manager baseUrl)
