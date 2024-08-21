module Kernel.External.Payment.Interface.Stripe where

import Control.Applicative ((<|>))
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Kernel.External.Encryption
import Kernel.External.Payment.Interface.Types
import Kernel.External.Payment.Stripe.Config as Reexport
import qualified Kernel.External.Payment.Stripe.Flow as Stripe
import qualified Kernel.External.Payment.Stripe.Types as Stripe
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common

createIndividualConnectAccount ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
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
    EncFlow m r
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
    EncFlow m r
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
    EncFlow m r
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
  return $ CreateCustomerResp {..}
  where
    mkCustomerReq :: CreateCustomerReq -> Stripe.CustomerReq
    mkCustomerReq CreateCustomerReq {..} = do
      let payment_method = Nothing
      let source = Nothing
      Stripe.CustomerReq {..}

createEphemeralKeys ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
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
    EncFlow m r
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
    EncFlow m r
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
    EncFlow m r
  ) =>
  StripeCfg ->
  CreatePaymentIntentReq ->
  m CreatePaymentIntentResp
createPaymentIntent config req = do
  let url = config.url
  apiKey <- decrypt config.apiKey
  let paymentIntentReq = mkPaymentIntentReq req
  paymentIntentResp <- Stripe.createPaymentIntent url apiKey paymentIntentReq
  let paymentIntentId = paymentIntentResp.id
  let clientSecret = paymentIntentResp.client_secret
  let status = paymentIntentResp.status
  return $ CreatePaymentIntentResp {..}
  where
    mkPaymentIntentReq :: CreatePaymentIntentReq -> Stripe.PaymentIntentReq
    mkPaymentIntentReq CreatePaymentIntentReq {amount = amonutInUsd, ..} = do
      let application_fee_amount = usdToCents applicationFeeAmount
      let amountInCents = usdToCents amonutInUsd
      let payment_method = paymentMethod
      let receipt_email = receiptEmail
      let on_behalf_of = driverAccountId
      let transfer_data = Stripe.TransferData {destination = driverAccountId}
      -- let automatic_payment_methods = Stripe.AutomaticPayementMethods {enabled = True, allow_redirects = Stripe.NeverRedirect}
      let confirm = True
      let description = Nothing
      let setup_future_usage = Nothing
      let capture_method = Stripe.ManualCaptureMethod
      let confirmation_method = Stripe.AutomaticConfirmationMethod
      let use_stripe_sdk = True
      let return_url = showBaseUrl config.returnUrl
      Stripe.PaymentIntentReq {amount = amountInCents, ..}

createSetupIntent ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
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
      let automatic_payment_methods = Stripe.AutomaticPayementMethods {enabled = True, allow_redirects = Stripe.NeverRedirect}
      let confirm = False
      let customer = customerId
      let description = Nothing
      let usage = Nothing
      let payment_method = Nothing
      let use_stripe_sdk = True
      Stripe.SetupIntentReq {..}

cancelPaymentIntent ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
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
    EncFlow m r
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
    EncFlow m r
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
    EncFlow m r
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
    EncFlow m r
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
    EncFlow m r
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
