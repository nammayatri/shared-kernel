module Kernel.Storage.InMem.Management.SidecarClient where

import Kernel.Prelude
import Kernel.Storage.InMem.Management.Types (RegisterKeyRequest (..), RegisterKeyResponse (..))
import qualified Network.HTTP.Client as HTTP
import Servant
import Servant.Client

type RegisterKeyAPI =
  "api"
    :> "registerKey"
    :> ReqBody '[JSON] RegisterKeyRequest
    :> Post '[JSON] RegisterKeyResponse

registerKeyClient :: RegisterKeyRequest -> ClientM RegisterKeyResponse
registerKeyClient = client (Proxy :: Proxy RegisterKeyAPI)

callRegisterKey :: HTTP.Manager -> BaseUrl -> RegisterKeyRequest -> IO (Either ClientError RegisterKeyResponse)
callRegisterKey manager baseUrl req =
  runClientM (registerKeyClient req) (mkClientEnv manager baseUrl)
