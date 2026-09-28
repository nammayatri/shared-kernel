module Kernel.External.VoiceSdk.BreezeBuddy.Flow where

import EulerHS.Types as Euler
import Kernel.External.VoiceSdk.BreezeBuddy.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

type CreateLeadAPI =
  "agent"
    :> "voice"
    :> "breeze-buddy"
    :> "leads"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] BreezeBuddyLeadRequest
    :> Post '[JSON] BreezeBuddyLeadResponse

type ConnectAPI =
  "agent"
    :> "voice"
    :> "breeze-buddy"
    :> "connect"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] BreezeBuddyConnectRequest
    :> Post '[JSON] BreezeBuddyConnectResponse

createLead ::
  (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) =>
  BaseUrl ->
  Text ->
  BreezeBuddyLeadRequest ->
  m BreezeBuddyLeadResponse
createLead url apiKey request = do
  let proxy = Proxy @CreateLeadAPI
      eulerClient = Euler.client proxy (Just ("Bearer " <> apiKey)) request
  callBreezeBuddyAPI url eulerClient "breeze-buddy-create-lead" proxy

connect ::
  (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) =>
  BaseUrl ->
  Text ->
  BreezeBuddyConnectRequest ->
  m BreezeBuddyConnectResponse
connect url apiKey request = do
  let proxy = Proxy @ConnectAPI
      eulerClient = Euler.client proxy (Just ("Bearer " <> apiKey)) request
  callBreezeBuddyAPI url eulerClient "breeze-buddy-connect" proxy

callBreezeBuddyAPI :: (MonadFlow m, HasRequestId r, MonadReader r m) => CallAPI' m r api res res
callBreezeBuddyAPI url eulerClient description proxy = do
  callAPI url eulerClient description proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call " <> description <> " API: " <> show err)
