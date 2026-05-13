{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payout.Stripe.Webhook
  ( PayoutStripeWebhookAPI,
    payoutServiceEventWebhook,
    RawByteString (..),
  )
where

import qualified Data.Aeson as A
import Kernel.External.Payment.Stripe.Types.Common (Event)
import Kernel.External.Payment.Stripe.Webhook (RawByteString (..), verifyStripeWebhookSignature)
import Kernel.External.Payout.Stripe.Config (StripeConfig (..))
import qualified Kernel.External.Payout.Stripe.Types.Webhook as PayoutWh
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import Kernel.Types.Error
import Kernel.Types.HideSecrets
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)

type PayoutStripeWebhookAPI =
  "service" :> "stripe" :> "payout"
    :> Header "Stripe-Signature" Text
    :> ReqBody '[OctetStream] RawByteString
    :> Post '[JSON] AckResponse

payoutServiceEventWebhook ::
  ( EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeConfig ->
  (Id Event -> m Bool) ->
  (PayoutWh.PayoutStripeWebhookReq -> Text -> m AckResponse) ->
  Maybe Text ->
  RawByteString ->
  m AckResponse
payoutServiceEventWebhook payoutStripeConfig checkDuplicatedEvent serviceEventHandler mbSigHeader rawBytes = do
  withLogTag "stripePayoutWebhook" $ do
    let mResp = A.eitherDecode (getRawByteString rawBytes)
    case mResp of
      Right (resp :: PayoutWh.PayoutStripeWebhookReq) -> withLogTag ("eventId-" <> resp.id.getId) $ do
        sigHeader <- mbSigHeader & fromMaybeM (InvalidRequest "Stripe-Signature header did not found")
        encryptedSecret <- payoutStripeConfig.webhookEndpointSecret & fromMaybeM (InternalError "STRIPE_PAYOUT_WEBHOOK_SECRET_NOT_FOUND")
        let tolerance = fromMaybe (Seconds 300) payoutStripeConfig.webhookToleranceSeconds
        void $ verifyStripeWebhookSignature encryptedSecret tolerance sigHeader rawBytes
        fork "stripe payout webhook" $ do
          isDuplicatedEvent <- checkDuplicatedEvent resp.id
          if not isDuplicatedEvent
            then do
              let respDump = encodeToText $ hideSecrets @PayoutWh.PayoutStripeWebhookReq resp
              void $ serviceEventHandler resp respDump
            else do
              logInfo $ "Duplicated Stripe payout webhook event found; skipping"
        pure Ack
      Left err -> do
        logInfo $ "Stripe payout webhook parsing failed: " <> show err
        throwError $ InvalidRequest "STRIPE_PAYOUT_WEBHOOK_PARSING_FAILED"
