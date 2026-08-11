{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Stripe.AccountWebhook
  ( AccountStripeWebhookAPI,
    accountServiceEventWebhook,
  )
where

import qualified Data.Aeson as A
import Kernel.External.Payment.Interface.Types (PaymentServiceConfig (..))
import qualified Kernel.External.Payment.Stripe.Types.AccountWebhook as AccountWh
import Kernel.External.Payment.Stripe.Types.Common (Event)
import Kernel.External.Payment.Stripe.Webhook (verifyStripeWebhookSignature)
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import Kernel.Types.Error
import Kernel.Types.HideSecrets
import Kernel.Types.Id
import Kernel.Types.Servant (RawByteString (..), RawJson)
import Kernel.Utils.Common
import Servant hiding (throwError)

type AccountStripeWebhookAPI =
  "service" :> "stripe" :> "account"
    :> Header "Stripe-Signature" Text
    :> ReqBody '[RawJson] RawByteString
    :> Post '[JSON] AckResponse

accountServiceEventWebhook ::
  ( EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  (Id Event -> m Bool) ->
  (AccountWh.AccountStripeWebhookReq -> Text -> m AckResponse) ->
  Maybe Text ->
  RawByteString ->
  m AckResponse
accountServiceEventWebhook paymentConfig checkDuplicatedEvent serviceEventHandler mbSigHeader rawBytes = do
  withLogTag "stripeAccountWebhook" $ do
    sigHeader <- mbSigHeader & fromMaybeM (InvalidRequest "Stripe-Signature header did not found")
    void $ verifyAuth paymentConfig sigHeader rawBytes
    let mResp = A.eitherDecode (getRawByteString rawBytes)
    case mResp of
      Right (resp :: AccountWh.AccountStripeWebhookReq) -> withLogTag ("eventId-" <> resp.id.getId) $ do
        fork "stripe account webhook" $ do
          isDuplicatedEvent <- checkDuplicatedEvent resp.id
          if not isDuplicatedEvent
            then do
              let respDump = encodeToText $ hideSecrets @AccountWh.AccountStripeWebhookReq resp
              void $ serviceEventHandler resp respDump
            else do
              logInfo $ "Duplicated Stripe account webhook event found; skipping"
        pure Ack
      Left err -> do
        logInfo $ "Stripe account webhook parsing failed: " <> show err
        throwError $ InvalidRequest "STRIPE_ACCOUNT_WEBHOOK_PARSING_FAILED"

verifyAuth ::
  EncFlow m r =>
  PaymentServiceConfig ->
  Text ->
  RawByteString ->
  m ()
verifyAuth config sigHeader rawBytes = case config of
  StripeConfig cfg -> do
    encryptedSecret <- cfg.webhookEndpointSecret & fromMaybeM (InternalError "STRIPE_WEBHOOK_SECRET_NOT_FOUND")
    let tolerance = fromMaybe 300 cfg.webhookToleranceSeconds
    verifyStripeWebhookSignature encryptedSecret tolerance sigHeader rawBytes
  _ -> throwError (InternalError "NOT_STRIPE_CONFIG")
