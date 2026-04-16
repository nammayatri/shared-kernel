module Kernel.External.Payout.Interface.Stripe
  ( createPayoutOrder,
    payoutOrderStatus,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Stripe as PaymentStripe
import Kernel.External.Payout.Interface.Types
import qualified Kernel.External.Payout.Juspay.Types.Payout as Juspay
import Kernel.External.Payout.Stripe.Config as Reexport
import qualified Kernel.External.Payout.Stripe.Flow as Stripe
import qualified Kernel.External.Payout.Stripe.Types as Stripe
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common

createPayoutOrder ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeConfig ->
  CreatePayoutOrderReq ->
  m CreatePayoutOrderResp
createPayoutOrder config req = do
  apiKey <- decrypt config.apiKey
  let url = config.url
  stripeResp <- Stripe.createPayout url apiKey req.mConnectedAccountId (mkCreatePayoutReq req)
  pure $ mkCreatePayoutOrderResp req.orderId (Just req) stripeResp
  where
    -- Interface request is payout-order shaped (Juspay), so map to Stripe payout request.
    mkCreatePayoutReq CreatePayoutOrderReq {..} =
      Stripe.CreatePayoutReq
        { amount = PaymentStripe.usdToCents amount,
          currency = T.toLower $ show currency,
          description = Just remark,
          destination = Just customerVpa,
          method = Nothing,
          source_type = Nothing,
          statement_descriptor = Nothing,
          metadata =
            Just
              Stripe.Metadata
                { order_id = Just orderId,
                  customer_id = Just customerId,
                  order_type = Just orderType
                }
        }

payoutOrderStatus ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeConfig ->
  PayoutOrderStatusReq ->
  m PayoutOrderStatusResp
payoutOrderStatus config req = do
  apiKey <- decrypt config.apiKey
  let url = config.url
  payoutId <- req.idAssignedByServiceProvider & fromMaybeM (InvalidRequest "id assigned by service provider required for Stripe payout")
  stripeResp <- Stripe.getPayout url apiKey req.mConnectedAccountId (Stripe.PayoutId payoutId)
  pure $ mkCreatePayoutOrderResp req.orderId Nothing stripeResp

mkCreatePayoutOrderResp :: Text -> Maybe CreatePayoutOrderReq -> Stripe.PayoutObject -> CreatePayoutOrderResp
mkCreatePayoutOrderResp reqOrderId mbRequest stripeResp =
  CreatePayoutOrderResp
    { orderId = fromMaybe reqOrderId $ stripeResp.metadata >>= (.order_id),
      idAssignedByServiceProvider = Just $ unPayoutId stripeResp.id,
      status = castPayoutStatus stripeResp.status,
      orderType = (stripeResp.metadata >>= (.order_type)) <|> (mbRequest <&> (.orderType)),
      udf1 = Nothing,
      udf2 = Nothing,
      udf3 = Nothing,
      udf4 = Nothing,
      udf5 = Nothing,
      amount = PaymentStripe.centsToUsd stripeResp.amount,
      refunds = Nothing,
      payments = Nothing,
      fulfillments = Nothing,
      customerId = (stripeResp.metadata >>= (.customer_id)) <|> (mbRequest <&> (.customerId))
    }

unPayoutId :: Stripe.PayoutId -> Text
unPayoutId (Stripe.PayoutId payoutId) = payoutId

castPayoutStatus :: Stripe.PayoutStatus -> Juspay.PayoutOrderStatus
castPayoutStatus = \case
  Stripe.PAYOUT_PENDING -> Juspay.INITIATED
  Stripe.PAYOUT_IN_TRANSIT -> Juspay.INITIATED
  Stripe.PAYOUT_PAID -> Juspay.SUCCESS
  Stripe.PAYOUT_FAILED -> Juspay.FAILURE
  Stripe.PAYOUT_CANCELED -> Juspay.CANCELLED
