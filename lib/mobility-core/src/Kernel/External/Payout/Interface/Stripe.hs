module Kernel.External.Payout.Interface.Stripe
  ( createExternalPayout,
    externalPayoutOrderStatus,
    createTransfer,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Kernel.External.Encryption
import Kernel.External.Payment.Interface.Stripe (centsToUsd, eurToCents, usdToCents)
import Kernel.External.Payout.Interface.Types
import Kernel.External.Payout.Stripe.Config as Reexport
import qualified Kernel.External.Payout.Stripe.Flow as Stripe
import qualified Kernel.External.Payout.Stripe.Types as Stripe
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common

createExternalPayout ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeConfig ->
  CreateExternalPayoutReq ->
  m CreateExternalPayoutResp
createExternalPayout config req = do
  apiKey <- decrypt config.apiKey
  let url = config.url
  stripeResp <- Stripe.createPayout url apiKey req.mConnectedAccountId (mkCreatePayoutReq req)
  pure $ mkCreateExternalPayoutResp req.orderId (Just req) stripeResp
  where
    mkCreatePayoutReq CreatePayoutOrderReq {..} =
      Stripe.CreatePayoutReq
        { amount = usdToCents amount,
          currency = T.toLower $ show currency,
          description = Just remark,
          destination = mExternalAccountId,
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

externalPayoutOrderStatus ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeConfig ->
  ExternalPayoutOrderStatusReq ->
  m ExternalPayoutOrderStatusResp
externalPayoutOrderStatus config req = do
  apiKey <- decrypt config.apiKey
  let url = config.url
  payoutId <- req.idAssignedByServiceProvider & fromMaybeM (InvalidRequest "id assigned by service provider required for Stripe payout")
  stripeResp <- Stripe.getPayout url apiKey req.mConnectedAccountId (Stripe.PayoutId payoutId)
  pure $ mkCreateExternalPayoutResp req.orderId Nothing stripeResp

mkCreateExternalPayoutResp :: Text -> Maybe CreateExternalPayoutReq -> Stripe.PayoutObject -> CreateExternalPayoutResp
mkCreateExternalPayoutResp reqOrderId mbRequest stripeResp =
  CreateExternalPayoutResp
    { orderId = fromMaybe reqOrderId $ stripeResp.metadata >>= (.order_id),
      externalPayoutStatus = Just stripeResp.status,
      idAssignedByServiceProvider = Just $ unPayoutId stripeResp.id,
      orderType = (stripeResp.metadata >>= (.order_type)) <|> (mbRequest <&> (.orderType)),
      amount = centsToUsd stripeResp.amount,
      customerId = (stripeResp.metadata >>= (.customer_id)) <|> (mbRequest <&> (.customerId))
    }

unPayoutId :: Stripe.PayoutId -> Text
unPayoutId (Stripe.PayoutId payoutId) = payoutId

createTransfer ::
  forall m r.
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeConfig ->
  CreateTransferReq ->
  m CreateTransferResp
createTransfer config req = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  transferReq <- buildCreateTransferReq req
  let senderAccountId = case req.senderAccountId of
        TransferConnectedAccount accountId -> Just accountId
        TransferPlatformAccount -> Nothing
  mkCreateTransferResp <$> Stripe.createTransfer url apiKey senderAccountId transferReq
  where
    buildCreateTransferReq :: CreateTransferReq -> m Stripe.TransferReq
    buildCreateTransferReq CreateTransferReq {amount = amountInUsd, ..} = do
      let amountInCents = eurToCents amountInUsd
      destination <- case destinationAccount of
        TransferConnectedAccount accountId -> pure accountId
        TransferPlatformAccount -> config.platformAccountId & fromMaybeM (InternalError "STRIPE_PLATFORM_ACCOUNT_ID_NOT_FOUND")
      pure Stripe.TransferReq {amount = amountInCents, metadata = Nothing, currency = T.toLower $ show currency, ..}

    mkCreateTransferResp :: Stripe.TransferObject -> CreateTransferResp
    mkCreateTransferResp Stripe.TransferObject {..} = CreateTransferResp {transferId = id}
