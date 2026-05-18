module Kernel.External.Payout.Interface.Stripe
  ( createExternalPayout,
    externalPayoutOrderStatus,
    createTransfer,
    payoutStripeServiceEventWebhook,
    castPayoutStatus,
    unPayoutId,
    module Reexport,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Kernel.External.Encryption
import Kernel.External.Payment.Interface.Stripe (centsToUsd, eurToCents, usdToCents)
import Kernel.External.Payment.Stripe.Types.Common (Event)
import Kernel.External.Payment.Stripe.Webhook (RawByteString (..))
import qualified Kernel.External.Payout.Interface.Events.Types as Events
import Kernel.External.Payout.Interface.Types as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as Juspay
import Kernel.External.Payout.Stripe.Config as Reexport
import qualified Kernel.External.Payout.Stripe.Flow as Stripe
import qualified Kernel.External.Payout.Stripe.Types as Stripe
import qualified Kernel.External.Payout.Stripe.Types.Webhook as PayoutWh
import qualified Kernel.External.Payout.Stripe.Webhook as PayoutStripeWh
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Beckn.Ack
import Kernel.Types.Error
import Kernel.Types.Id
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
      status = castPayoutStatus stripeResp.status,
      idAssignedByServiceProvider = Just $ unPayoutId stripeResp.id,
      orderType = (stripeResp.metadata >>= (.order_type)) <|> (mbRequest <&> (.orderType)),
      amount = centsToUsd stripeResp.amount,
      customerId = (stripeResp.metadata >>= (.customer_id)) <|> (mbRequest <&> (.customerId))
    }

unPayoutId :: Stripe.PayoutId -> Text
unPayoutId (Stripe.PayoutId payoutId) = payoutId

castPayoutStatus :: Stripe.ExternalPayoutStatus -> Juspay.PayoutOrderStatus
castPayoutStatus = \case
  Stripe.EXTERNAL_PAYOUT_PENDING -> Juspay.INITIATED
  Stripe.EXTERNAL_PAYOUT_IN_TRANSIT -> Juspay.INITIATED
  Stripe.EXTERNAL_PAYOUT_PAID -> Juspay.SUCCESS
  Stripe.EXTERNAL_PAYOUT_FAILED -> Juspay.FAILURE
  Stripe.EXTERNAL_PAYOUT_CANCELED -> Juspay.CANCELLED

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
    mkCreateTransferResp Stripe.TransferObject {..} = CreateTransferResp {transferId = id, transferStatus = TRANSFERRED}

payoutStripeServiceEventWebhook ::
  ( EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PayoutServiceConfig ->
  (Id Event -> m Bool) ->
  (Events.PayoutServiceEventResp -> Text -> m AckResponse) ->
  Maybe Text ->
  RawByteString ->
  m AckResponse
payoutStripeServiceEventWebhook serviceConfig checkDuplicatedEvent serviceEventHandler =
  PayoutStripeWh.payoutServiceEventWebhook serviceConfig checkDuplicatedEvent (\resp respDump -> buildPayoutServiceEventResp resp >>= flip serviceEventHandler respDump)

buildPayoutServiceEventResp :: (MonadThrow m, Log m) => PayoutWh.PayoutStripeWebhookReq -> m Events.PayoutServiceEventResp
buildPayoutServiceEventResp PayoutWh.PayoutStripeWebhookReq {..} = do
  eventData <- buildPayoutEventObject _type _data._object
  pure
    Events.PayoutServiceEventResp
      { id,
        apiVersion = api_version,
        createdAt = posixSecondsToUTCTime created,
        eventData,
        livemode,
        pendingWebhooks = pending_webhooks,
        eventType = _type,
        ..
      }

buildPayoutEventObject :: (MonadThrow m, Log m) => PayoutWh.PayoutStripeWebhookEventType -> PayoutWh.PayoutStripeWebhookObject -> m Events.EventObject
buildPayoutEventObject eventType stripeObject = case (eventType, stripeObject) of
  (PayoutWh.PayoutCanceled, PayoutWh.ObjectPayout obj) -> pure $ Events.PayoutCanceledEvent $ mkPayoutObject obj
  (PayoutWh.PayoutCreated, PayoutWh.ObjectPayout obj) -> pure $ Events.PayoutCreatedEvent $ mkPayoutObject obj
  (PayoutWh.PayoutFailed, PayoutWh.ObjectPayout obj) -> pure $ Events.PayoutFailedEvent $ mkPayoutObject obj
  (PayoutWh.PayoutPaid, PayoutWh.ObjectPayout obj) -> pure $ Events.PayoutPaidEvent $ mkPayoutObject obj
  (PayoutWh.PayoutReconciliationCompleted, PayoutWh.ObjectPayout obj) -> pure $ Events.PayoutReconciliationCompletedEvent $ mkPayoutObject obj
  (PayoutWh.PayoutUpdated, PayoutWh.ObjectPayout obj) -> pure $ Events.PayoutUpdatedEvent $ mkPayoutObject obj
  (PayoutWh.TransferCreated, PayoutWh.ObjectTransfer obj) -> pure $ Events.TransferCreatedEvent $ mkTransferObject obj
  (PayoutWh.TransferReversed, PayoutWh.ObjectTransfer obj) -> pure $ Events.TransferReversedEvent $ mkTransferObject obj
  (PayoutWh.TransferUpdated, PayoutWh.ObjectTransfer obj) -> pure $ Events.TransferUpdatedEvent $ mkTransferObject obj
  (PayoutWh.PayoutStripeWebhookCustomEvent eventName, PayoutWh.PayoutStripeWebhookCustomObject _ _) -> pure $ Events.CustomEvent eventName
  (_, _) ->
    throwError $
      InvalidRequest $
        "Invalid object: "
          <> PayoutWh.getPayoutStripeWebhookObjectType stripeObject
          <> " found for event: "
          <> PayoutWh.payoutStripeWebhookEventTypeToText eventType

mkPayoutObject :: Stripe.PayoutObject -> Events.Payout
mkPayoutObject Stripe.PayoutObject {..} =
  Events.Payout
    { payoutId = id,
      orderId = metadata >>= (.order_id),
      customerId = metadata >>= (.customer_id),
      orderType = metadata >>= (.order_type),
      amount = centsToUsd amount,
      arrivalDate = posixSecondsToUTCTime <$> arrival_date,
      createdAt = posixSecondsToUTCTime created,
      statementDescriptor = statement_descriptor,
      failureCode = failure_code,
      failureMessage = failure_message,
      payoutType = _type,
      ..
    }

mkTransferObject :: Stripe.TransferObject -> Events.Transfer
mkTransferObject Stripe.TransferObject {..} =
  Events.Transfer
    { transferId = id,
      amount = centsToUsd amount,
      createdAt = posixSecondsToUTCTime created,
      ..
    }
