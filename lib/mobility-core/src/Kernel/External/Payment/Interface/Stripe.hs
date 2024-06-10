module Kernel.External.Payment.Interface.Stripe where

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
  let accountReq = mkAccountReq req
  accountResp <- Stripe.createAccount url apiKey accountReq
  let accountId = accountResp.id
  let chargesEnabled = accountResp.charges_enabled
  let detailsSubmitted = accountResp.details_submitted
  let accountLinkReq = mkAccountLinkReq config accountId
  accountLinkResp <- Stripe.createAccountLink url apiKey accountLinkReq
  let accountUrl = accountLinkResp.url
  let accountUrlExpiry = posixSecondsToUTCTime accountLinkResp.expires_at
  pure $ IndividualConnectAccountResp {..}

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

mkAccountReq :: IndividualConnectAccountReq -> Stripe.AccountsReq
mkAccountReq req =
  let _type = Stripe.Standard
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
              cashapp_payments = Stripe.CashAppPayments {requested = True}
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
   in Stripe.AccountsReq {..}

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
