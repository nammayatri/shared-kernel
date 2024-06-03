module Kernel.External.Payment.Stripe.Flow where

import qualified Data.Text.Encoding as DT
import EulerHS.Types as Euler
import Kernel.External.Payment.Stripe.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CallAPI', callAPI, fromEitherM)
import Servant hiding (throwError)

callStripeAPI :: CallAPI' m api res res
callStripeAPI url eulerClient description proxy = do
  callAPI url eulerClient description proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call " <> description <> " API: " <> show err)

mkBasicAuthData :: Text -> BasicAuthData
mkBasicAuthData apiKey =
  BasicAuthData
    { basicAuthUsername = DT.encodeUtf8 apiKey,
      basicAuthPassword = ""
    }

-- https://docs.stripe.com/api/accounts/create

type CreateAccountAPI =
  "v1"
    :> "accounts"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[JSON] AccountsReq
    :> Post '[JSON] AccountResp

createAccount ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  AccountsReq ->
  m AccountResp
createAccount url apiKey req = do
  let proxy = Proxy @CreateAccountAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) req
  callStripeAPI url eulerClient "create-account" proxy

type CreateAccountLinkAPI =
  "v1"
    :> "accounts"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[JSON] AccountLinkReq
    :> Post '[JSON] AccountLinkObject

createAccountLink ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  AccountLinkReq ->
  m AccountLinkObject
createAccountLink url apiKey req = do
  let proxy = Proxy @CreateAccountLinkAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) req
  callStripeAPI url eulerClient "create-account-link" proxy

type GetAccountAPI =
  "v1"
    :> "accounts"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Capture "id" AccountId
    :> Get '[JSON] AccountResp

getAccount ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  AccountId ->
  m AccountResp
getAccount url apiKey accountId = do
  let proxy = Proxy @GetAccountAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) accountId
  callStripeAPI url eulerClient "get-account" proxy
