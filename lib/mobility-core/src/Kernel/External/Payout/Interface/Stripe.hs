module Kernel.External.Payout.Interface.Stripe
  ( createPayoutOrder,
    payoutOrderStatus,
  )
where

import Kernel.External.Encryption
import Kernel.External.Payout.Interface.Types
import qualified Kernel.External.Payout.Juspay.Types.Payout as Juspay
import Kernel.External.Payout.Stripe.Config as Reexport
import qualified Kernel.External.Payout.Stripe.Flow as Stripe
import qualified Kernel.External.Payout.Stripe.Types as Stripe
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Servant.Client

createPayoutOrder ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeConfig ->
  Maybe Text ->
  CreatePayoutOrderReq ->
  m CreatePayoutOrderResp
createPayoutOrder config mConnectedAccountId req = do
  apiKey <- decrypt config.apiKey
  let url = config.url
  stripeResp <- Stripe.createPayout url apiKey mConnectedAccountId (mkCreatePayoutReq req)
  pure $ mkCreatePayoutOrderResp (Just req) stripeResp
  where
    -- Interface request is payout-order shaped (Juspay), so map to Stripe payout request.
    mkCreatePayoutReq CreatePayoutOrderReq {..} =
      Stripe.CreatePayoutReq
        { amount = round amount,
          currency = "inr",
          description = Just remark,
          destination = Just customerVpa,
          method = Nothing,
          sourceType = Nothing,
          statementDescriptor = Nothing,
          metadata = Just Stripe.Metadata {order_id = Just orderId}
        }

payoutOrderStatus ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeConfig ->
  Text ->
  Maybe Text ->
  m PayoutOrderStatusResp
payoutOrderStatus config payoutId mConnectedAccountId = do
  apiKey <- decrypt config.apiKey
  let url = config.url
  stripeResp <- Stripe.getPayout url apiKey mConnectedAccountId (Stripe.PayoutId payoutId)
  pure $ mkCreatePayoutOrderResp Nothing stripeResp

mkCreatePayoutOrderResp :: Maybe CreatePayoutOrderReq -> Stripe.PayoutObject -> CreatePayoutOrderResp
mkCreatePayoutOrderResp mbRequest stripeResp =
  CreatePayoutOrderResp
    { orderId = unPayoutId stripeResp.id,
      status = mkPayoutOrderStatus stripeResp.status,
      orderType = (.orderType) <$> mbRequest,
      udf1 = Nothing,
      udf2 = Nothing,
      udf3 = Nothing,
      udf4 = Nothing,
      udf5 = Nothing,
      amount = fromIntegral stripeResp.amount,
      refunds = Nothing,
      payments = Nothing,
      fulfillments = Nothing,
      customerId = (.customerId) <$> mbRequest
    }

unPayoutId :: Stripe.PayoutId -> Text
unPayoutId (Stripe.PayoutId payoutId) = payoutId

mkPayoutOrderStatus :: Stripe.PayoutStatus -> Juspay.PayoutOrderStatus
mkPayoutOrderStatus = \case
  Stripe.PAYOUT_PENDING -> Juspay.INITIATED
  Stripe.PAYOUT_IN_TRANSIT -> Juspay.INITIATED
  Stripe.PAYOUT_PAID -> Juspay.SUCCESS
  Stripe.PAYOUT_FAILED -> Juspay.FAILURE
  Stripe.PAYOUT_CANCELED -> Juspay.CANCELLED
