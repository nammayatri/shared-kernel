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

-------------------------------------------- Connect Account APIs --------------------------------------------
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

-------------------------------------------- Customer APIs --------------------------------------------
type CreateCustomerAPI =
  "v1"
    :> "customers"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[JSON] CustomerReq
    :> Post '[JSON] CustomerObject

createCustomer ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  CustomerReq ->
  m CustomerObject
createCustomer url apiKey customerReq = do
  let proxy = Proxy @CreateCustomerAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) customerReq
  callStripeAPI url eulerClient "create-customer" proxy

type UpdateCustomerAPI =
  "v1"
    :> "customers"
    :> Capture "id" CustomerId
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[JSON] UpdateCustomerReq
    :> Post '[JSON] CustomerObject

updateCustomer ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  CustomerId ->
  UpdateCustomerReq ->
  m CustomerObject
updateCustomer url apiKey customerId customerReq = do
  let proxy = Proxy @UpdateCustomerAPI
      eulerClient = Euler.client proxy customerId (mkBasicAuthData apiKey) customerReq
  callStripeAPI url eulerClient "update-customer" proxy

type GetCustomerAPI =
  "v1"
    :> "customers"
    :> Capture "id" CustomerId
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Get '[JSON] CustomerObject

getCustomer ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  CustomerId ->
  m CustomerObject
getCustomer url apiKey customerId = do
  let proxy = Proxy @GetCustomerAPI
      eulerClient = Euler.client proxy customerId (mkBasicAuthData apiKey)
  callStripeAPI url eulerClient "get-customer" proxy

-------------------------------------------- Payment Intent APIs --------------------------------------------
type CreatePaymentIntentAPI =
  "v1"
    :> "payment_intents"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[JSON] CreatePaymentIntentReq
    :> Post '[JSON] PaymentIntentObject

createPaymentIntent ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  CreatePaymentIntentReq ->
  m PaymentIntentObject
createPaymentIntent url apiKey paymentIntentReq = do
  let proxy = Proxy @CreatePaymentIntentAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) paymentIntentReq
  callStripeAPI url eulerClient "create-payment-intent" proxy

-------------------------------------------- Card APIs --------------------------------------------
type CreateCardAPI =
  "v1"
    :> "customers"
    :> Capture "id" CustomerId
    :> "sources"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[JSON] CardReq
    :> Post '[JSON] CardObject

createCard ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  CustomerId ->
  CardReq ->
  m CardObject
createCard url apiKey customerId cardReq = do
  let proxy = Proxy @CreateCardAPI
      eulerClient = Euler.client proxy customerId (mkBasicAuthData apiKey) cardReq
  callStripeAPI url eulerClient "create-card" proxy

type UpdateCardAPI =
  "v1"
    :> "customers"
    :> Capture "id" CustomerId
    :> "sources"
    :> Capture "card_id" PaymentMethodId
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[JSON] UpdateCardReq
    :> Post '[JSON] CardObject

updateCard ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  CustomerId ->
  PaymentMethodId ->
  UpdateCardReq ->
  m CardObject
updateCard url apiKey customerId cardId updateCardReq = do
  let proxy = Proxy @UpdateCardAPI
      eulerClient = Euler.client proxy customerId cardId (mkBasicAuthData apiKey) updateCardReq
  callStripeAPI url eulerClient "update-card" proxy

type GetCardAPI =
  "v1"
    :> "customers"
    :> Capture "id" CustomerId
    :> "cards"
    :> Capture "card_id" PaymentMethodId
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Get '[JSON] CardObject

getCard ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  CustomerId ->
  PaymentMethodId ->
  m CardObject
getCard url apiKey customerId cardId = do
  let proxy = Proxy @GetCardAPI
      eulerClient = Euler.client proxy customerId cardId (mkBasicAuthData apiKey)
  callStripeAPI url eulerClient "get-card" proxy

-------------------------------------------- Ephemeral Keys APIs --------------------------------------------
type CreateEphemeralKeysAPI =
  "v1"
    :> "ephemeral_keys"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[JSON] EphemeralKeysReq
    :> Post '[JSON] CardObject

createEphemeralKeys ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  EphemeralKeysReq ->
  m CardObject
createEphemeralKeys url apiKey ephemeralKeysReq = do
  let proxy = Proxy @CreateEphemeralKeysAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) ephemeralKeysReq
  callStripeAPI url eulerClient "create-ephemeralKeys" proxy
