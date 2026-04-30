module Kernel.External.PartnerSdk.Aarokya.Flow where

import EulerHS.Types as Euler
import Kernel.External.PartnerSdk.Aarokya.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

type GenerateTokenAPI =
  "auth"
    :> "token"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] AarokyaTokenRequest
    :> Post '[JSON] AarokyaTokenResponse

generateToken ::
  (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) =>
  BaseUrl ->
  Text ->
  AarokyaTokenRequest ->
  m AarokyaTokenResponse
generateToken url basicToken request = do
  let proxy = Proxy @GenerateTokenAPI
      eulerClient = Euler.client proxy (Just ("Basic " <> basicToken)) request
  callAarokyaAPI url eulerClient "aarokya-generate-token" proxy

callAarokyaAPI :: (MonadFlow m, HasRequestId r, MonadReader r m) => CallAPI' m r api res res
callAarokyaAPI url eulerClient description proxy = do
  callAPI url eulerClient description proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call " <> description <> " API: " <> show err)
