module Kernel.External.Payout.Stripe.Flow where

import qualified EulerHS.Types as Euler
import qualified Kernel.External.Payment.Stripe.Flow as PaymentFlow
import Kernel.External.Payout.Stripe.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Utils.Servant.Client
import Servant hiding (throwError)

-- Create Payout API
type CreatePayoutAPI =
  "v1"
    :> "payouts"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Header "Stripe-Account" Text -- Optional connected account ID
    :> ReqBody '[FormUrlEncoded] CreatePayoutReq
    :> Post '[JSON] PayoutObject

createPayout ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  Maybe Text ->
  CreatePayoutReq ->
  m PayoutObject
createPayout url apiKey connectedAccountId payoutReq = do
  let proxy = Proxy @CreatePayoutAPI
      eulerClient = Euler.client proxy (PaymentFlow.mkBasicAuthData apiKey) connectedAccountId payoutReq
  PaymentFlow.callStripeAPI url eulerClient "create-payout" proxy

-- Get Payout API
type GetPayoutAPI =
  "v1"
    :> "payouts"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Header "Stripe-Account" Text
    :> Capture "id" Text
    :> Get '[JSON] PayoutObject

getPayout ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  Maybe Text ->
  PayoutId ->
  m PayoutObject
getPayout url apiKey connectedAccountId (PayoutId payoutId) = do
  let proxy = Proxy @GetPayoutAPI
      eulerClient = Euler.client proxy (PaymentFlow.mkBasicAuthData apiKey) connectedAccountId payoutId
  PaymentFlow.callStripeAPI url eulerClient "get-payout" proxy

-- Cancel Payout API
type CancelPayoutAPI =
  "v1"
    :> "payouts"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Header "Stripe-Account" Text
    :> Capture "id" Text
    :> "cancel"
    :> Post '[JSON] PayoutObject

cancelPayout ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  Maybe Text ->
  PayoutId ->
  m PayoutObject
cancelPayout url apiKey connectedAccountId (PayoutId payoutId) = do
  let proxy = Proxy @CancelPayoutAPI
      eulerClient = Euler.client proxy (PaymentFlow.mkBasicAuthData apiKey) connectedAccountId payoutId
  PaymentFlow.callStripeAPI url eulerClient "cancel-payout" proxy

-- List Payouts API
type ListPayoutsAPI =
  "v1"
    :> "payouts"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Header "Stripe-Account" Text
    :> QueryParam "limit" Int
    :> QueryParam "starting_after" Text
    :> QueryParam "ending_before" Text
    :> QueryParam "status" PayoutStatus
    :> Get '[JSON] PayoutList

listPayouts ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  Maybe Text ->
  Maybe Int -> -- limit
  Maybe Text -> -- starting_after
  Maybe Text -> -- ending_before
  Maybe PayoutStatus -> -- status filter
  m PayoutList
listPayouts url apiKey connectedAccountId limit startingAfter endingBefore status = do
  let proxy = Proxy @ListPayoutsAPI
      eulerClient = Euler.client proxy (PaymentFlow.mkBasicAuthData apiKey) connectedAccountId limit startingAfter endingBefore status
  PaymentFlow.callStripeAPI url eulerClient "list-payouts" proxy

-- CRUD apis for exernal accounts

-- List External Accounts (READ)
type ListExternalAccountsAPI =
  "v1"
    :> "accounts"
    :> Capture "account_id" AccountId
    :> "external_accounts"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> QueryParam "object" Text -- "bank_account" or "card"
    :> QueryParam "limit" Int
    :> QueryParam "starting_after" Text
    :> QueryParam "ending_before" Text
    :> Get '[JSON] ExternalAccountList

listExternalAccounts ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  AccountId ->
  Maybe Text -> -- "bank_account" or "card"
  Maybe Int -> -- limit
  Maybe Text -> -- starting_after
  Maybe Text -> -- ending_before
  m ExternalAccountList
listExternalAccounts url apiKey accountId objectType limit startingAfter endingBefore = do
  let proxy = Proxy @ListExternalAccountsAPI
      eulerClient = Euler.client proxy accountId (PaymentFlow.mkBasicAuthData apiKey) objectType limit startingAfter endingBefore
  PaymentFlow.callStripeAPI url eulerClient "list-external-accounts" proxy

-- Create External Account (CREATE)
type CreateExternalAccountAPI =
  "v1"
    :> "accounts"
    :> Capture "account_id" AccountId
    :> "external_accounts"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[FormUrlEncoded] ExternalAccountReq
    :> Post '[JSON] ExternalAccountObject

createExternalAccount ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  AccountId ->
  ExternalAccountReq ->
  m ExternalAccountObject
createExternalAccount url apiKey accountId externalAccountReq = do
  let proxy = Proxy @CreateExternalAccountAPI
      eulerClient = Euler.client proxy accountId (PaymentFlow.mkBasicAuthData apiKey) externalAccountReq
  PaymentFlow.callStripeAPI url eulerClient "create-external-account" proxy

-- Get External Account (READ)
type GetExternalAccountAPI =
  "v1"
    :> "accounts"
    :> Capture "account_id" AccountId
    :> "external_accounts"
    :> Capture "id" Text
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Get '[JSON] ExternalAccountObject

getExternalAccount ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  AccountId ->
  ExternalAccountId ->
  m ExternalAccountObject
getExternalAccount url apiKey accountId (ExternalAccountId externalAccountId) = do
  let proxy = Proxy @GetExternalAccountAPI
      eulerClient = Euler.client proxy accountId externalAccountId (PaymentFlow.mkBasicAuthData apiKey)
  PaymentFlow.callStripeAPI url eulerClient "get-external-account" proxy

-- Update External Account (UPDATE)
type UpdateExternalAccountAPI =
  "v1"
    :> "accounts"
    :> Capture "account_id" AccountId
    :> "external_accounts"
    :> Capture "id" Text
    :> BasicAuth "secretkey-password" BasicAuthData
    :> ReqBody '[FormUrlEncoded] UpdateExternalAccountReq
    :> Post '[JSON] ExternalAccountObject

updateExternalAccount ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  AccountId ->
  ExternalAccountId ->
  UpdateExternalAccountReq ->
  m ExternalAccountObject
updateExternalAccount url apiKey accountId (ExternalAccountId externalAccountId) updateReq = do
  let proxy = Proxy @UpdateExternalAccountAPI
      eulerClient = Euler.client proxy accountId externalAccountId (PaymentFlow.mkBasicAuthData apiKey) updateReq
  PaymentFlow.callStripeAPI url eulerClient "update-external-account" proxy

-- Delete External Account (DELETE)
type DeleteExternalAccountAPI =
  "v1"
    :> "accounts"
    :> Capture "account_id" AccountId
    :> "external_accounts"
    :> Capture "id" Text
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Delete '[JSON] DeletedExternalAccount

deleteExternalAccount ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  AccountId ->
  ExternalAccountId ->
  m DeletedExternalAccount
deleteExternalAccount url apiKey accountId (ExternalAccountId externalAccountId) = do
  let proxy = Proxy @DeleteExternalAccountAPI
      eulerClient = Euler.client proxy accountId externalAccountId (PaymentFlow.mkBasicAuthData apiKey)
  PaymentFlow.callStripeAPI url eulerClient "delete-external-account" proxy
