module Kernel.External.Payment.Stripe.Flow where

import qualified Data.Text.Encoding as DT
import EulerHS.Types as Euler
import Kernel.External.Payment.Stripe.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Utils.Common (CallAPI, callApiUnwrappingApiError)
import Servant hiding (throwError)

callStripeAPI :: CallAPI m api res
callStripeAPI url eulerClient description proxy = do
  callApiUnwrappingApiError (identity @StripeError) Nothing Nothing Nothing url eulerClient description proxy

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
    :> ReqBody '[FormUrlEncoded] AccountsReq
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
    :> "account_links"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[FormUrlEncoded] AccountLinkReq
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
    :> ReqBody '[FormUrlEncoded] CustomerReq
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
    :> ReqBody '[FormUrlEncoded] UpdateCustomerReq
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
    :> ReqBody '[FormUrlEncoded] PaymentIntentReq
    :> Post '[JSON] PaymentIntentObject

createPaymentIntent ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  PaymentIntentReq ->
  m PaymentIntentObject
createPaymentIntent url apiKey paymentIntentReq = do
  let proxy = Proxy @CreatePaymentIntentAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) paymentIntentReq
  callStripeAPI url eulerClient "create-payment-intent" proxy

type CancelPaymentIntentAPI =
  "v1"
    :> "payment_intents"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Capture "id" PaymentIntentId
    :> "cancel"
    :> Post '[JSON] PaymentIntentObject

cancelPaymentIntent ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  PaymentIntentId ->
  m PaymentIntentObject
cancelPaymentIntent url apiKey paymentIntentId = do
  let proxy = Proxy @CancelPaymentIntentAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) paymentIntentId
  callStripeAPI url eulerClient "cancel-payment-intent" proxy

type GetPaymentIntentAPI =
  "v1"
    :> "payment_intents"
    :> Capture "id" PaymentIntentId
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Get '[JSON] PaymentIntentObject

getPaymentIntent ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  PaymentIntentId ->
  m PaymentIntentObject
getPaymentIntent url apiKey paymentIntentId = do
  let proxy = Proxy @GetPaymentIntentAPI
      eulerClient = Euler.client proxy paymentIntentId (mkBasicAuthData apiKey)
  callStripeAPI url eulerClient "get-payment-intent" proxy

type ConfirmPaymentIntentAPI =
  "v1"
    :> "payment_intents"
    :> Capture "id" PaymentIntentId
    :> "confirm"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[FormUrlEncoded] ConfirmPaymentIntentReq
    :> Post '[JSON] PaymentIntentObject

confirmPaymentIntent ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  PaymentIntentId ->
  ConfirmPaymentIntentReq ->
  m PaymentIntentObject
confirmPaymentIntent url apiKey paymentIntentId confirmPaymentIntentReq = do
  let proxy = Proxy @ConfirmPaymentIntentAPI
      eulerClient = Euler.client proxy paymentIntentId (mkBasicAuthData apiKey) confirmPaymentIntentReq
  callStripeAPI url eulerClient "confirm-payment-intent" proxy

type CapturePaymentIntentAPI =
  "v1"
    :> "payment_intents"
    :> Capture "id" PaymentIntentId
    :> "capture"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[FormUrlEncoded] CapturePaymentIntentReq
    :> Post '[JSON] PaymentIntentObject

capturePaymentIntent ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  PaymentIntentId ->
  CapturePaymentIntentReq ->
  m PaymentIntentObject
capturePaymentIntent url apiKey paymentIntentId capturePaymentIntentReq = do
  let proxy = Proxy @CapturePaymentIntentAPI
      eulerClient = Euler.client proxy paymentIntentId (mkBasicAuthData apiKey) capturePaymentIntentReq
  callStripeAPI url eulerClient "capture-payment-intent" proxy

type IncrementAuthorizationPaymentIntentAPI =
  "v1"
    :> "payment_intents"
    :> Capture "id" PaymentIntentId
    :> "increment_authorization"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[FormUrlEncoded] IncrementAuthorizationReq
    :> Post '[JSON] PaymentIntentObject

incrementAuthorizationPaymentIntent ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  PaymentIntentId ->
  IncrementAuthorizationReq ->
  m PaymentIntentObject
incrementAuthorizationPaymentIntent url apiKey paymentIntentId incrementAuthorizationReq = do
  let proxy = Proxy @IncrementAuthorizationPaymentIntentAPI
      eulerClient = Euler.client proxy paymentIntentId (mkBasicAuthData apiKey) incrementAuthorizationReq
  callStripeAPI url eulerClient "increment-authorization-payment-intent" proxy

-------------------------------------------- Setup Intent APIs --------------------------------------------
type CreateSetupIntentAPI =
  "v1"
    :> "setup_intents"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[FormUrlEncoded] SetupIntentReq
    :> Post '[JSON] SetupIntentObject

createSetupIntent ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  SetupIntentReq ->
  m SetupIntentObject
createSetupIntent url apiKey setupIntentReq = do
  let proxy = Proxy @CreateSetupIntentAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) setupIntentReq
  callStripeAPI url eulerClient "create-setup-intent" proxy

-------------------------------------------- Card APIs --------------------------------------------
type CreateCardAPI =
  "v1"
    :> "customers"
    :> Capture "id" CustomerId
    :> "sources"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[FormUrlEncoded] CardReq
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

type DeleteCardAPI =
  "v1"
    :> "customers"
    :> Capture "id" CustomerId
    :> "sources"
    :> Capture "card_id" PaymentMethodId
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Delete '[JSON] DeleteCardResp

deleteCard ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  CustomerId ->
  PaymentMethodId ->
  m DeleteCardResp
deleteCard url apiKey customerId cardId = do
  let proxy = Proxy @DeleteCardAPI
      eulerClient = Euler.client proxy customerId cardId (mkBasicAuthData apiKey)
  callStripeAPI url eulerClient "delete-card" proxy

type UpdateCardAPI =
  "v1"
    :> "customers"
    :> Capture "id" CustomerId
    :> "sources"
    :> Capture "card_id" PaymentMethodId
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[FormUrlEncoded] UpdateCardReq
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

type GetCardListAPI =
  "v1"
    :> "customers"
    :> Capture "id" CustomerId
    :> "cards"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Get '[JSON] CardList

getCardList ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  CustomerId ->
  m CardList
getCardList url apiKey customerId = do
  let proxy = Proxy @GetCardListAPI
      eulerClient = Euler.client proxy customerId (mkBasicAuthData apiKey)
  callStripeAPI url eulerClient "get-card-list" proxy

type GetPaymentMethodListAPI =
  "v1"
    :> "customers"
    :> Capture "id" CustomerId
    :> "payment_methods"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Get '[JSON] PaymentMethodList

getPaymentMethodList ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  CustomerId ->
  m PaymentMethodList
getPaymentMethodList url apiKey customerId = do
  let proxy = Proxy @GetPaymentMethodListAPI
      eulerClient = Euler.client proxy customerId (mkBasicAuthData apiKey)
  callStripeAPI url eulerClient "get-payment-method-list" proxy

type DetachPaymentMethodAPI =
  "v1"
    :> "payment_methods"
    :> Capture "id" PaymentMethodId
    :> "detach"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Post '[JSON] PaymentMethod

detachPaymentMethod ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  PaymentMethodId ->
  m PaymentMethod
detachPaymentMethod url apiKey paymentMethodId = do
  let proxy = Proxy @DetachPaymentMethodAPI
      eulerClient = Euler.client proxy paymentMethodId (mkBasicAuthData apiKey)
  callStripeAPI url eulerClient "detach-payment-method" proxy

-------------------------------------------- Ephemeral Keys APIs --------------------------------------------
type CreateEphemeralKeysAPI =
  "v1"
    :> "ephemeral_keys"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Header "Stripe-Version" Text
    :> ReqBody '[FormUrlEncoded] EphemeralKeysReq
    :> Post '[JSON] EphemeralKeysResp

createEphemeralKeys ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  EphemeralKeysReq ->
  m EphemeralKeysResp
createEphemeralKeys url apiKey ephemeralKeysReq = do
  let proxy = Proxy @CreateEphemeralKeysAPI
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) (Just "2024-04-10") ephemeralKeysReq
  callStripeAPI url eulerClient "create-ephemeralKeys" proxy
