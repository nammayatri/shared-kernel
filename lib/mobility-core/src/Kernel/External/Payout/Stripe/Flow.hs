module Kernel.External.Payout.Stripe.Flow where

import qualified Data.Text.Encoding as DT
import qualified EulerHS.Types as Euler
import Kernel.External.Payment.Stripe.Types (StripeError)
import Kernel.External.Payout.Stripe.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Utils.Servant.Client
import Servant hiding (throwError)

-- TODO reuse
callStripeAPI :: CallAPI m r api res
callStripeAPI url eulerClient description proxy = do
  callApiUnwrappingApiError (identity @StripeError) Nothing Nothing Nothing url eulerClient description proxy

-- TODO reuse
mkBasicAuthData :: Text -> BasicAuthData
mkBasicAuthData apiKey =
  BasicAuthData
    { basicAuthUsername = DT.encodeUtf8 apiKey,
      basicAuthPassword = ""
    }

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
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) connectedAccountId payoutReq
  callStripeAPI url eulerClient "create-payout" proxy

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
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) connectedAccountId payoutId
  callStripeAPI url eulerClient "get-payout" proxy

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
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) connectedAccountId payoutId
  callStripeAPI url eulerClient "cancel-payout" proxy

-- List Payouts API
type ListPayoutsAPI =
  "v1"
    :> "payouts"
    :> BasicAuth "secretkey-password" BasicAuthData
    :> Header "Stripe-Account" Text
    :> QueryParam "limit" Int
    :> QueryParam "starting_after" Text
    :> QueryParam "ending_before" Text
    :> QueryParam "status" Text
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
      statusText = fmap payoutStatusToText status
      eulerClient = Euler.client proxy (mkBasicAuthData apiKey) connectedAccountId limit startingAfter endingBefore statusText
  callStripeAPI url eulerClient "list-payouts" proxy
  where
    payoutStatusToText PayoutPending = "pending"
    payoutStatusToText PayoutInTransit = "in_transit"
    payoutStatusToText PayoutPaid = "paid"
    payoutStatusToText PayoutFailed = "failed"
    payoutStatusToText PayoutCanceled = "canceled"
