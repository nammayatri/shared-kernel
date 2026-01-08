module Kernel.External.Payment.Interface.Stripe
  ( module Kernel.External.Payment.Interface.Stripe,
    module Reexport,
  )
where

import Control.Applicative ((<|>))
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Events.Types as Events
import Kernel.External.Payment.Interface.Types
import Kernel.External.Payment.Stripe.Config as Reexport
import qualified Kernel.External.Payment.Stripe.Flow as Stripe
import qualified Kernel.External.Payment.Stripe.Types as Stripe
import qualified Kernel.External.Payment.Stripe.Webhook as Stripe
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common

createIndividualConnectAccount ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  IndividualConnectAccountReq ->
  m IndividualConnectAccountResp
createIndividualConnectAccount config req = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  let accountReq = mkAccountReq
  accountResp <- Stripe.createAccount url apiKey accountReq
  let accountId = accountResp.id
  let chargesEnabled = accountResp.charges_enabled
  let detailsSubmitted = accountResp.details_submitted
  let accountLinkReq = mkAccountLinkReq config accountId
  accountLinkResp <- Stripe.createAccountLink url apiKey accountLinkReq
  let accountUrl = accountLinkResp.url
  let accountUrlExpiry = posixSecondsToUTCTime accountLinkResp.expires_at
  pure $ IndividualConnectAccountResp {..}
  where
    mkAccountReq :: Stripe.AccountsReq
    mkAccountReq =
      let _type = Nothing
          country =
            case req.country of
              Context.India -> "IN"
              Context.USA -> "US"
              Context.France -> "FR"
              Context.Netherlands -> "NL"
              Context.Finland -> "FI"
              Context.AnyCountry -> "US" -- fix later
          email = req.email
          controller =
            Just $
              Stripe.AccountController
                { fees = Just $ Stripe.AccountFees {payer = Stripe.AccountFeePayerAccount},
                  losses = Just $ Stripe.AccountLosses {payments = Stripe.AccountLossesPayerStripe},
                  requirement_collection = Just Stripe.AccountRquirementCollectorStripe,
                  stripe_dashboard = Just $ Stripe.AccountDashboard {_type = Stripe.AccountDashboardNone}
                }
          capabilities =
            Just $
              Stripe.AccountCapabilities
                { card_payments = Stripe.CardPayments {requested = True},
                  -- cashapp_payments = Stripe.CashAppPayments {requested = True}
                  transfers = Stripe.Transfers {requested = True}
                }
          settings =
            Just $
              Stripe.AccountSettings
                { payouts = Stripe.PayoutsSettings {debit_negative_balances = True, statement_descriptor = "Bridge Rideshare"}
                }
          business_type = Stripe.Individual
          (year', month, day) = toGregorian req.dateOfBirth
          individual =
            Just $
              Stripe.IndividualDetails
                { first_name = req.firstName,
                  last_name = req.lastName,
                  dob = Just $ Stripe.DateOfBirth {year = fromInteger year', ..},
                  address = req.address,
                  email = req.email,
                  id_number = req.idNumber,
                  phone = req.mobileNumber,
                  ssn_last_4 = req.ssnLast4
                }
          default_business_profile =
            Just $
              Stripe.BusinessProfile
                { mcc = Just "4121",
                  product_description = Just "Rideshare driver",
                  support_phone = Just "7605636815", -- dummy number
                  url = Just "https://bridge.cab",
                  support_address =
                    Just $
                      Stripe.BusinessSupportAddress
                        { city = Just "St. Louis Park",
                          country = Just "US",
                          line1 = Just "Suite 100, 1650, West End Blvd",
                          line2 = Nothing,
                          postal_code = Just "55416",
                          state = Just "MN"
                        }
                }
          business_profile = config.businessProfile <|> default_business_profile
       in Stripe.AccountsReq {..}

retryAccountLink ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  Stripe.AccountId ->
  m RetryAccountLink
retryAccountLink config accountId = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  let accountLinkReq = mkAccountLinkReq config accountId
  accountLinkResp <- Stripe.createAccountLink url apiKey accountLinkReq
  let accountUrlExpiry = posixSecondsToUTCTime accountLinkResp.expires_at
  let accountUrl = accountLinkResp.url
  pure $ RetryAccountLink {..}

getAccount ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  Stripe.AccountId ->
  m ConnectAccountResp
getAccount config accountId = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  accountResp <- Stripe.getAccount url apiKey accountId
  let chargesEnabled = accountResp.charges_enabled
  let detailsSubmitted = accountResp.details_submitted
  pure $ ConnectAccountResp {..}

createCustomer ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  CreateCustomerReq ->
  m CreateCustomerResp
createCustomer config req = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  let customerReq = mkCustomerReq req
  customerResp <- Stripe.createCustomer url apiKey customerReq
  let customerId = customerResp.id
  let clientAuthToken = Nothing
  let clientAuthTokenExpiry = Nothing
  return $ CreateCustomerResp {..}
  where
    mkCustomerReq :: CreateCustomerReq -> Stripe.CustomerReq
    mkCustomerReq CreateCustomerReq {..} = do
      Stripe.CustomerReq
        { email = fromMaybe "User@gmail.com" email,
          name = fromMaybe "User" name,
          payment_method = Nothing,
          source = Nothing,
          phone = phone
        }

createEphemeralKeys ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  CustomerId ->
  m Text
createEphemeralKeys config customerId = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  let ephemeralKeysReq = Stripe.EphemeralKeysReq {customer = customerId}
  ephemeralKeysResp <- Stripe.createEphemeralKeys url apiKey ephemeralKeysReq
  return ephemeralKeysResp.secret

getCardList ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  CustomerId ->
  m CustomerCardListResp
getCardList config customerId = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  paymentMethodListResp <- Stripe.getPaymentMethodList url apiKey customerId
  let cards = map mkCard paymentMethodListResp._data
  return cards
  where
    mkCard :: Stripe.PaymentMethod -> CustomerCard
    mkCard paymentMethod =
      CustomerCard
        { cardId = paymentMethod.id,
          expMonth = paymentMethod.card.exp_month,
          expYear = paymentMethod.card.exp_year,
          last4 = paymentMethod.card.last4,
          brand = paymentMethod.card.brand,
          country = paymentMethod.card.country
        }

deleteCard ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  PaymentMethodId ->
  m ()
deleteCard config paymentMethodId = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  void $ Stripe.detachPaymentMethod url apiKey paymentMethodId

createPaymentIntent ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  CreatePaymentIntentReq ->
  m CreatePaymentIntentResp
createPaymentIntent config req = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  case config.chargeDestination of
    -- Platform receives payment, transfers to driver (Destination Charges)
    Platform -> createPlatformCharge url apiKey req
    -- Driver receives payment directly (Direct Charges)
    ConnectedAccount -> createConnectedAccountCharge url apiKey req
  where
    -- Platform Charge: No cloning, no on_behalf_of
    createPlatformCharge url apiKey CreatePaymentIntentReq {amount = amonutInUsd, ..} = do
      let paymentIntentReq = mkPlatformPaymentIntentReq
      paymentIntentResp <- Stripe.createPaymentIntent url apiKey paymentIntentReq
      let paymentIntentId = paymentIntentResp.id
      let clientSecret = paymentIntentResp.client_secret
      let status = paymentIntentResp.status
      return $ CreatePaymentIntentResp {..}
      where
        mkPlatformPaymentIntentReq :: Stripe.PaymentIntentReq
        mkPlatformPaymentIntentReq =
          let application_fee_amount = eurToCents applicationFeeAmount
              amountInCents = eurToCents amonutInUsd
              payment_method = paymentMethod -- Use original payment method (NO cloning)
              receipt_email = receiptEmail
              on_behalf_of = Nothing -- OMIT for platform charges
              transfer_data = Stripe.TransferData {destination = driverAccountId}
              confirm = True
              description = Nothing
              setup_future_usage = Nothing
              capture_method = Stripe.ManualCaptureMethod
              confirmation_method = Stripe.AutomaticConfirmationMethod
              use_stripe_sdk = True
              return_url = showBaseUrl config.returnUrl
              metadata = Metadata {order_short_id = Just orderShortId, order_id = Nothing, refunds_id = Nothing}
           in Stripe.PaymentIntentReq {amount = amountInCents, ..}

    -- Connected Account Charge: Clone payment method, use on_behalf_of
    createConnectedAccountCharge url apiKey reqData = do
      -- Clone the payment method to the driver's connected account
      let clonePaymentMethodReq = Stripe.ClonePaymentMethodReq {payment_method = reqData.paymentMethod, customer = reqData.customer}
      clonedPMRes <- Stripe.clonePaymentMethod url apiKey reqData.driverAccountId clonePaymentMethodReq
      -- use cloned paymentMethodId to create paymentIntent
      let paymentIntentReq = mkPaymentIntentReq clonedPMRes.id reqData
      paymentIntentResp <- Stripe.createPaymentIntent url apiKey paymentIntentReq
      let paymentIntentId = paymentIntentResp.id
      let clientSecret = paymentIntentResp.client_secret
      let status = paymentIntentResp.status
      return $ CreatePaymentIntentResp {..}
      where
        mkPaymentIntentReq :: PaymentMethodId -> CreatePaymentIntentReq -> Stripe.PaymentIntentReq
        mkPaymentIntentReq clonedPaymentMethodId CreatePaymentIntentReq {amount = amonutInUsd, ..} = do
          let application_fee_amount = usdToCents applicationFeeAmount
          let amountInCents = usdToCents amonutInUsd
          let payment_method = clonedPaymentMethodId
          let receipt_email = receiptEmail
          let on_behalf_of = Just driverAccountId
          let transfer_data = Stripe.TransferData {destination = driverAccountId}
          -- let automatic_payment_methods = Stripe.AutomaticPaymentMethods {enabled = True, allow_redirects = Stripe.NeverRedirect}
          let confirm = True
          let description = Nothing
          let setup_future_usage = Nothing
          let capture_method = Stripe.ManualCaptureMethod
          let confirmation_method = Stripe.AutomaticConfirmationMethod
          let use_stripe_sdk = True
          let return_url = showBaseUrl config.returnUrl
          let metadata = Metadata {order_short_id = Just orderShortId, order_id = Nothing, refunds_id = Nothing}
          Stripe.PaymentIntentReq {amount = amountInCents, ..}

createSetupIntent ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  CustomerId ->
  m CreateSetupIntentResp
createSetupIntent config customerId = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  let setupIntentReq = mkSetupIntentReq
  setupIntentResp <- Stripe.createSetupIntent url apiKey setupIntentReq
  let setupIntentId = setupIntentResp.id
  let clientSecret = setupIntentResp.client_secret
  let status = setupIntentResp.status
  return $ CreateSetupIntentResp {..}
  where
    mkSetupIntentReq :: Stripe.SetupIntentReq
    mkSetupIntentReq = do
      let automatic_payment_methods = Stripe.AutomaticPaymentMethods {enabled = True, allow_redirects = Stripe.NeverRedirect}
      let confirm = False
      let customer = customerId
      let description = Nothing
      let usage = Nothing
      let payment_method = Nothing
      let use_stripe_sdk = True
      Stripe.SetupIntentReq {..}

cancelPaymentIntent ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  PaymentIntentId ->
  m CreatePaymentIntentResp
cancelPaymentIntent config paymentIntentId = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  paymentIntentResp <- Stripe.cancelPaymentIntent url apiKey paymentIntentId
  let clientSecret = paymentIntentResp.client_secret
  let status = paymentIntentResp.status
  return $ CreatePaymentIntentResp {..}

updatePaymentMethodInIntent ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  PaymentIntentId ->
  PaymentMethodId ->
  m ()
updatePaymentMethodInIntent config paymentIntentId paymentMethodId = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  let confirmPaymentIntentReq = Stripe.ConfirmPaymentIntentReq {payment_method = paymentMethodId}
  void $ Stripe.confirmPaymentIntent url apiKey paymentIntentId confirmPaymentIntentReq

getCard ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  PaymentMethodId ->
  CustomerId ->
  m CustomerCard
getCard config paymentMethodId customerId = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  cardObjectResp <- Stripe.getCard url apiKey customerId paymentMethodId
  let card = mkCard cardObjectResp
  return card
  where
    mkCard :: Stripe.CardObject -> CustomerCard
    mkCard cardObject =
      CustomerCard
        { cardId = cardObject.id,
          expMonth = cardObject.exp_month,
          expYear = cardObject.exp_year,
          last4 = cardObject.last4,
          brand = cardObject.brand,
          country = cardObject.country
        }

getPaymentIntent ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  PaymentIntentId ->
  m CreatePaymentIntentResp
getPaymentIntent config paymentIntentId = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  paymentIntentResp <- Stripe.getPaymentIntent url apiKey paymentIntentId
  let clientSecret = paymentIntentResp.client_secret
  let status = paymentIntentResp.status
  return $ CreatePaymentIntentResp {..}

capturePaymentIntent ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  PaymentIntentId ->
  HighPrecMoney ->
  HighPrecMoney ->
  m ()
capturePaymentIntent config paymentIntentId amount applicationFeeAmount = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  let amount_to_capture = usdToCents amount
  let application_fee_amount = usdToCents applicationFeeAmount
  let req = Stripe.CapturePaymentIntentReq {..}
  void $ Stripe.capturePaymentIntent url apiKey paymentIntentId req

updateAmountInPaymentIntent ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  PaymentIntentId ->
  HighPrecMoney ->
  HighPrecMoney ->
  m ()
updateAmountInPaymentIntent config paymentIntentId amount_ applicationFeeAmount = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  let amount = usdToCents amount_
  let application_fee_amount = usdToCents applicationFeeAmount
  let req = Stripe.IncrementAuthorizationReq {..}
  void $ Stripe.incrementAuthorizationPaymentIntent url apiKey paymentIntentId req

mkAccountLinkReq :: StripeCfg -> Stripe.AccountId -> Stripe.AccountLinkReq
mkAccountLinkReq config accountId =
  let account = accountId
      refresh_url = showBaseUrl config.refreshUrl
      return_url = showBaseUrl config.returnUrl
      _type = Stripe.AccountOnboarding
      collection_options =
        Just $
          Stripe.CollectionOptions
            { fields = Stripe.CurrentlyDue,
              future_requirements = Stripe.Omit
            }
   in Stripe.AccountLinkReq {..}

-- TODO: Do it properly later for other currencies as well

-- | Convert USD to cents
usdToCents :: HighPrecMoney -> Int
usdToCents (HighPrecMoney money) = round $ money * 100

eurToCents :: HighPrecMoney -> Int
eurToCents (HighPrecMoney money) = round $ money * 100

-- | Convert cents to USD
centsToUsd :: Int -> HighPrecMoney
centsToUsd cents = HighPrecMoney $ (toRational cents / 100)

serviceEventWebhook ::
  ( EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PaymentServiceConfig ->
  (Id Stripe.Event -> m Bool) ->
  (Events.ServiceEventResp -> Text -> m AckResponse) ->
  Maybe Text ->
  Stripe.RawByteString ->
  m AckResponse
serviceEventWebhook paymentConfig checkDuplicatedEvent serviceEventHandler mbSigHeader rawBytes = do
  Stripe.serviceEventWebhook paymentConfig checkDuplicatedEvent (\resp respDump -> buildServiceEventResp resp >>= flip serviceEventHandler respDump) mbSigHeader rawBytes

buildServiceEventResp :: (MonadThrow m, Log m) => Stripe.WebhookReq -> m Events.ServiceEventResp
buildServiceEventResp Stripe.WebhookReq {..} = do
  eventData <- buildEventObject _type _data._object
  pure
    Events.ServiceEventResp
      { id,
        apiVersion = api_version,
        createdAt = posixSecondsToUTCTime created,
        eventData,
        livemode,
        pendingWebhooks = pending_webhooks,
        eventType = _type,
        ..
      }

buildEventObject :: (MonadThrow m, Log m) => Stripe.EventType -> Stripe.StripeObject -> m Events.EventObject
buildEventObject eventType stripeObject = case (eventType, stripeObject) of
  (Stripe.PaymentIntentSucceeded, Stripe.ObjectPaymentIntent obj) -> pure $ Events.PaymentIntentSucceededEvent $ mkPaymentIntentObject obj
  (Stripe.PaymentIntentPaymentFailed, Stripe.ObjectPaymentIntent obj) -> pure $ Events.PaymentIntentPaymentFailedEvent $ mkPaymentIntentObject obj
  (Stripe.PaymentIntentProcessing, Stripe.ObjectPaymentIntent obj) -> pure $ Events.PaymentIntentProcessingEvent $ mkPaymentIntentObject obj
  (Stripe.PaymentIntentCanceled, Stripe.ObjectPaymentIntent obj) -> pure $ Events.PaymentIntentCanceledEvent $ mkPaymentIntentObject obj
  (Stripe.PaymentIntentCreated, Stripe.ObjectPaymentIntent obj) -> pure $ Events.PaymentIntentCreatedEvent $ mkPaymentIntentObject obj
  (Stripe.PaymentIntentRequiresAction, Stripe.ObjectPaymentIntent obj) -> pure $ Events.PaymentIntentRequiresActionEvent $ mkPaymentIntentObject obj
  (Stripe.SetupIntentSucceeded, Stripe.ObjectSetupIntent obj) -> pure $ Events.SetupIntentSucceededEvent $ mkSetupIntentObject obj
  (Stripe.SetupIntentSetupFailed, Stripe.ObjectSetupIntent obj) -> pure $ Events.SetupIntentSetupFailedEvent $ mkSetupIntentObject obj
  (Stripe.SetupIntentCanceled, Stripe.ObjectSetupIntent obj) -> pure $ Events.SetupIntentCanceledEvent $ mkSetupIntentObject obj
  (Stripe.SetupIntentCreated, Stripe.ObjectSetupIntent obj) -> pure $ Events.SetupIntentCreatedEvent $ mkSetupIntentObject obj
  (Stripe.SetupIntentRequiresAction, Stripe.ObjectSetupIntent obj) -> pure $ Events.SetupIntentRequiresActionEvent $ mkSetupIntentObject obj
  (Stripe.ChargeSucceeded, Stripe.ObjectCharge obj) -> pure $ Events.ChargeSucceededEvent $ mkChargeObject obj
  (Stripe.ChargeFailed, Stripe.ObjectCharge obj) -> pure $ Events.ChargeFailedEvent $ mkChargeObject obj
  (Stripe.ChargeRefunded, Stripe.ObjectCharge obj) -> pure $ Events.ChargeRefundedEvent $ mkChargeObject obj
  (Stripe.ChargeDisputeCreated, Stripe.ObjectCharge obj) -> pure $ Events.ChargeDisputeCreatedEvent $ mkChargeObject obj
  (Stripe.ChargeDisputeClosed, Stripe.ObjectCharge obj) -> pure $ Events.ChargeDisputeClosedEvent $ mkChargeObject obj
  (Stripe.ChargeRefundUpdated, Stripe.ObjectRefund obj) -> pure $ Events.ChargeRefundUpdatedEvent $ mkGetRefundResp obj
  (Stripe.RefundCreated, Stripe.ObjectRefund obj) -> pure $ Events.RefundCreatedEvent $ mkGetRefundResp obj
  (Stripe.RefundUpdated, Stripe.ObjectRefund obj) -> pure $ Events.RefundUpdatedEvent $ mkGetRefundResp obj
  (Stripe.RefundFailed, Stripe.ObjectRefund obj) -> pure $ Events.RefundFailedEvent $ mkGetRefundResp obj
  (Stripe.CustomEvent eventName, Stripe.CustomObject _objectName _obj) -> pure $ Events.CustomEvent eventName
  (_, _) -> throwError (InvalidRequest $ "Invalid object: " <> Stripe.getObjectType stripeObject <> "found for event:" <> Stripe.eventTypeToText eventType)

mkPaymentIntentObject :: Stripe.PaymentIntent -> Events.PaymentIntent
mkPaymentIntentObject Stripe.PaymentIntent {..} =
  Events.PaymentIntent
    { paymentIntentId = id,
      orderShortId = metadata >>= (.order_short_id),
      amount = centsToUsd amount,
      amountCapturable = centsToUsd amount_capturable,
      amountReceived = centsToUsd amount_received,
      applicationFeeAmount = centsToUsd <$> application_fee_amount,
      automaticPaymentMethods = castAutomaticPaymentMethods <$> automatic_payment_methods,
      canceledAt = posixSecondsToUTCTime <$> canceled_at,
      cancellationReason = cancellation_reason,
      captureMethod = capture_method,
      clientSecret = client_secret,
      confirmationMethod = confirmation_method,
      createdAt = posixSecondsToUTCTime created,
      lastPaymentError = last_payment_error,
      latestCharge = latest_charge,
      nextAction = next_action,
      onBehalfOf = on_behalf_of,
      paymentMethod = payment_method,
      paymentMethodOptions = payment_method_options,
      paymentMethodTypes = payment_method_types,
      receiptEmail = receipt_email,
      setupFutureUsage = setup_future_usage,
      transferGroup = transfer_group,
      ..
    }

castAutomaticPaymentMethods :: Stripe.AutomaticPaymentMethods -> Events.AutomaticPaymentMethods
castAutomaticPaymentMethods Stripe.AutomaticPaymentMethods {..} =
  Events.AutomaticPaymentMethods
    { allowRedirects = allow_redirects,
      ..
    }

mkSetupIntentObject :: Stripe.SetupIntent -> Events.SetupIntent
mkSetupIntentObject Stripe.SetupIntent {..} =
  Events.SetupIntent
    { automaticPaymentMethods = castAutomaticPaymentMethods <$> automatic_payment_methods,
      cancellationReason = cancellation_reason,
      clientSecret = client_secret,
      createdAt = posixSecondsToUTCTime created,
      flowDirections = flow_directions,
      lastSetupError = last_setup_error,
      latestAttempt = latest_attempt,
      nextAction = next_action,
      onBehalfOf = on_behalf_of,
      paymentMethod = payment_method,
      paymentMethodOptions = castPaymentMethodOptions <$> payment_method_options,
      paymentMethodTypes = payment_method_types,
      singleUseMandate = single_use_mandate,
      ..
    }

castPaymentMethodOptions :: Stripe.PaymentMethodOptions -> Events.PaymentMethodOptions
castPaymentMethodOptions Stripe.PaymentMethodOptions {..} =
  Events.PaymentMethodOptions
    { acssDebit = castACSSDebit <$> acss_debit
    }

castACSSDebit :: Stripe.ACSSDebit -> Events.ACSSDebit
castACSSDebit Stripe.ACSSDebit {..} =
  Events.ACSSDebit
    { mandateOptions = castMandateOptions mandate_options,
      verificationMethod = verification_method,
      ..
    }

castMandateOptions :: Stripe.MandateOptions -> Events.MandateOptions
castMandateOptions Stripe.MandateOptions {..} =
  Events.MandateOptions
    { intervalDescription = interval_description,
      paymentSchedule = payment_schedule,
      transactionType = transaction_type
    }

mkChargeObject :: Stripe.Charge -> Events.Charge
mkChargeObject Stripe.Charge {..} =
  Events.Charge
    { chargeId = id,
      orderShortId = metadata >>= (.order_short_id),
      amount = centsToUsd amount,
      amountCaptured = centsToUsd amount_captured,
      amountRefunded = centsToUsd amount_refunded,
      applicationFeeAmount = centsToUsd <$> application_fee_amount,
      balanceTransaction = balance_transaction,
      calculatedStatementDescriptor = calculated_statement_descriptor,
      createdAt = posixSecondsToUTCTime created,
      failureCode = failure_code,
      failureMessage = failure_message,
      fraudDetails = fraud_details,
      paymentIntentId = payment_intent,
      paymentMethod = payment_method,
      receiptEmail = receipt_email,
      receiptUrl = receipt_url,
      ..
    }

createRefund ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  CreateRefundReq ->
  m CreateRefundResp
createRefund config req = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  case config.chargeDestination of
    -- Platform receives payment, transfers to driver (Destination Charges)
    Platform -> createPlatformRefund url apiKey
    -- Driver receives payment directly (Direct Charges)
    ConnectedAccount -> createConnectedAccountRefund url apiKey
  where
    -- Platform Charge: Need to reverse transfer
    createPlatformRefund url apiKey = do
      let reverseTransfer = Just True
          refundReq = mkRefundReq req reverseTransfer
      mkRefundResp <$> Stripe.createRefund url apiKey Nothing refundReq

    -- Connected Account Charge: Need to send driver account id in header
    createConnectedAccountRefund url apiKey = do
      let reverseTransfer = Nothing
          refundReq = mkRefundReq req reverseTransfer
      mkRefundResp <$> Stripe.createRefund url apiKey (Just req.driverAccountId) refundReq

    mkRefundReq :: CreateRefundReq -> Maybe Bool -> Stripe.RefundReq
    mkRefundReq CreateRefundReq {amount = amonutInUsd, ..} reverse_transfer =
      let charge = Nothing
          payment_intent = Just req.paymentIntentId
          amountInCents = eurToCents <$> amonutInUsd
          metadata = Metadata {order_short_id = Just orderShortId, order_id = Just orderId, refunds_id = Just refundsId}
          refund_application_fee = Just req.refundApplicationFee
          instructions_email = req.email
       in Stripe.RefundReq {amount = amountInCents, reason = Just Stripe.REQUESTED_BY_CUSTOMER, ..}

    mkRefundResp :: Stripe.RefundObject -> CreateRefundResp
    mkRefundResp Stripe.RefundObject {..} =
      let reverseTransferId = transfer_reversal
       in CreateRefundResp {status = castRefundStatus status, errorCode = failure_reason, ..}

castRefundStatus :: Stripe.RefundStatus -> RefundStatus
castRefundStatus = \case
  Stripe.REFUND_SUCCEEDED -> REFUND_SUCCESS
  Stripe.REFUND_PENDING -> REFUND_PENDING
  Stripe.REFUND_FAILED -> REFUND_FAILURE
  Stripe.REFUND_CANCELED -> REFUND_CANCELED
  Stripe.REFUND_REQUIRES_ACTION -> REFUND_REQUIRES_ACTION

getRefund ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  GetRefundReq ->
  m GetRefundResp
getRefund config req = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  case config.chargeDestination of
    Platform -> mkGetRefundResp <$> Stripe.getRefund url apiKey Nothing req.id
    ConnectedAccount -> mkGetRefundResp <$> Stripe.getRefund url apiKey (Just req.driverAccountId) req.id

cancelRefund ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeCfg ->
  CancelRefundReq ->
  m CancelRefundResp
cancelRefund config req = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  case config.chargeDestination of
    Platform -> mkGetRefundResp <$> Stripe.cancelRefund url apiKey Nothing req.id
    ConnectedAccount -> mkGetRefundResp <$> Stripe.cancelRefund url apiKey (Just req.driverAccountId) req.id

mkGetRefundResp :: Stripe.RefundObject -> GetRefundResp
mkGetRefundResp Stripe.RefundObject {..} =
  GetRefundResp
    { id,
      orderShortId = metadata >>= (.order_short_id),
      orderId = metadata >>= (.order_id),
      refundsId = metadata >>= (.refunds_id),
      paymentIntentId = payment_intent,
      amount = centsToUsd amount,
      currency,
      status = castRefundStatus status,
      reverseTransferId = transfer_reversal,
      errorCode = failure_reason
    }
