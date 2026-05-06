module Kernel.External.Payout.Interface.Stripe
  ( createPayoutOrder,
    payoutOrderStatus,
    listExternalAccounts,
    createExternalAccount,
    getExternalAccount,
    updateExternalAccount,
    deleteExternalAccount,
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

listExternalAccounts ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeConfig ->
  ListExternalAccountsReq ->
  m ListExternalAccountsResp
listExternalAccounts config ListExternalAccountsReq {..} = do
  apiKey <- decrypt config.apiKey
  stripeResp <- Stripe.listExternalAccounts config.url apiKey accountId objectType limit startingAfter endingBefore
  pure $ mkListExternalAccountsResp stripeResp

createExternalAccount ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeConfig ->
  CreateExternalAccountReq ->
  m CreateExternalAccountResp
createExternalAccount config CreateExternalAccountReq {..} = do
  apiKey <- decrypt config.apiKey
  stripeResp <- Stripe.createExternalAccount config.url apiKey accountId (mkStripeExternalAccountReq CreateExternalAccountReq {..})
  pure $ mkExternalAccountResp stripeResp

getExternalAccount ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeConfig ->
  GetExternalAccountReq ->
  m GetExternalAccountResp
getExternalAccount config GetExternalAccountReq {..} = do
  apiKey <- decrypt config.apiKey
  stripeResp <- Stripe.getExternalAccount config.url apiKey accountId (Stripe.ExternalAccountId externalAccountId)
  pure $ mkExternalAccountResp stripeResp

updateExternalAccount ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeConfig ->
  UpdateExternalAccountReq ->
  m UpdateExternalAccountResp
updateExternalAccount config UpdateExternalAccountReq {..} = do
  apiKey <- decrypt config.apiKey
  stripeResp <- Stripe.updateExternalAccount config.url apiKey accountId (Stripe.ExternalAccountId externalAccountId) (mkStripeUpdateExternalAccountReq UpdateExternalAccountReq {..})
  pure $ mkExternalAccountResp stripeResp

deleteExternalAccount ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  StripeConfig ->
  DeleteExternalAccountReq ->
  m DeleteExternalAccountResp
deleteExternalAccount config DeleteExternalAccountReq {..} = do
  apiKey <- decrypt config.apiKey
  stripeResp <- Stripe.deleteExternalAccount config.url apiKey accountId (Stripe.ExternalAccountId externalAccountId)
  pure $ mkDeleteExternalAccountResp stripeResp

mkStripeExternalAccountReq :: CreateExternalAccountReq -> Stripe.ExternalAccountReq
mkStripeExternalAccountReq CreateExternalAccountReq {..} =
  Stripe.ExternalAccountReq
    { _object = externalAccountObject,
      country = externalAccountCountry,
      currency = T.toLower $ show externalAccountCurrency,
      account_number = externalBankAccountNumber,
      routing_number = externalBankRoutingNumber,
      number = externalAccountNumber,
      exp_month = externalAccountExpMonth,
      exp_year = externalAccountExpYear,
      cvc = externalAccountCvc,
      default_for_currency = externalAccountDefaultForCurrency,
      metadata = externalAccountMetadata
    }

mkStripeUpdateExternalAccountReq :: UpdateExternalAccountReq -> Stripe.UpdateExternalAccountReq
mkStripeUpdateExternalAccountReq UpdateExternalAccountReq {..} =
  Stripe.UpdateExternalAccountReq
    { default_for_currency = externalAccountDefaultForCurrency,
      metadata = externalAccountMetadata
    }

mkExternalAccountResp :: Stripe.ExternalAccountObject -> ExternalAccount
mkExternalAccountResp Stripe.ExternalAccountObject {..} =
  ExternalAccount
    { id = id,
      externalAccountObject = _object,
      account = account,
      bankName = bank_name,
      country = country,
      -- currency = currency,
      defaultForCurrency = default_for_currency,
      last4 = last4,
      status = status
    }

mkListExternalAccountsResp :: Stripe.ExternalAccountList -> ListExternalAccountsResp
mkListExternalAccountsResp Stripe.ExternalAccountList {..} =
  ListExternalAccountsResp
    { accounts = mkExternalAccountResp <$> _data,
      hasMore = has_more
    }

mkDeleteExternalAccountResp :: Stripe.DeletedExternalAccount -> DeleteExternalAccountResp
mkDeleteExternalAccountResp Stripe.DeletedExternalAccount {..} =
  DeleteExternalAccountResp
    { externalAccountId = id,
      externalAccountObject = _object,
      externalAccountDeleted = deleted
    }
