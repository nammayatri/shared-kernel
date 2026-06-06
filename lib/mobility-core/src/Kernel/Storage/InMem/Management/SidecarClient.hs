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

-- | Retry an IO action on ConnectionError with exponential backoff.
-- Retries up to maxAttempts times with delays of 100ms, 200ms, 400ms, ...
callWithRetry :: Int -> IO (Either ClientError a) -> IO (Either ClientError a)
callWithRetry maxAttempts action = go 1
  where
    go attempt = do
      result <- action
      case result of
        Left (ConnectionError _)
          | attempt < maxAttempts -> do
            threadDelay (100000 * (2 ^ (attempt - 1))) -- 100ms * 2^(attempt-1)
            go (attempt + 1)
        _ -> pure result

callRegisterKey :: HTTP.Manager -> BaseUrl -> RegisterKeyRequest -> IO (Either ClientError RegisterKeyResponse)
callRegisterKey manager baseUrl req =
  callWithRetry 3 $
    runClientM (registerKeyClient req) (mkClientEnv manager baseUrl)

callRefresh :: HTTP.Manager -> BaseUrl -> SidecarRefreshRequest -> IO (Either ClientError SidecarRefreshResponse)
callRefresh manager baseUrl req =
  callWithRetry 3 $
    runClientM (refreshClient req) (mkClientEnv manager baseUrl)
