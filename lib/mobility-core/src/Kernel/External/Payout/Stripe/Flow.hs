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
