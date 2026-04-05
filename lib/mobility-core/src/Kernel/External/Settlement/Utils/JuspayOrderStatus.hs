{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.Utils.JuspayOrderStatus
  ( fetchOrderStatus,
    parsePaymentOrderId,
    isOfferTransaction,
    extractPaymentRrn,
    enrichPaymentReport,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Juspay.Flow as JuspayFlow
import Kernel.External.Payment.Juspay.Types.Common (OrderStatusResp)
import qualified Kernel.External.Payment.Juspay.Types.Common as Juspay
import Kernel.External.Settlement.Interface.Types
import Kernel.External.Settlement.Types (SettlementServiceConfig (..), SplitSettlementCustomerType (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Utils.Servant.Client (HasRequestId)

-- ---------------------------------------------------------------------------
-- Fetch order status from Juspay
-- ---------------------------------------------------------------------------

fetchOrderStatus ::
  ( EncFlow m r,
    Metrics.CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SettlementServiceConfig ->
  Text ->
  m (Maybe OrderStatusResp)
fetchOrderStatus config orderId = do
  let enabled = fromMaybe False config.juspayOrderStatusEnabled
  if not enabled
    then pure Nothing
    else case (config.juspayBaseUrl, config.juspayApiKey) of
      (Just baseUrlText, Just encApiKey) ->
        case parsePaymentOrderId orderId of
          Nothing -> pure Nothing
          Just (merchantId, orderShortId) -> do
            apiKeyPlain <- decrypt encApiKey
            baseUrl' <- parseBaseUrl baseUrlText
            let mRoutingId = Nothing
            result <-
              try @_ @SomeException $
                JuspayFlow.orderStatus baseUrl' apiKeyPlain merchantId mRoutingId orderShortId
            liftIO $ threadDelay 300000
            case result of
              Left _err -> pure Nothing
              Right resp -> pure (Just resp)
      _ -> pure Nothing

-- ---------------------------------------------------------------------------
-- Parse payment order ID
-- ---------------------------------------------------------------------------

-- | Parse a payment order ID into (merchantId, orderShortId).
--
-- Format: @merchant-[env-]shortId-suffix@
--
--   * @cumta-Z8chrCxq3N-1@                -> @("cumta", "Z8chrCxq3N")@
--   * @nammayatriBAP-test-5pMXJR6YPt-1@   -> @("nammayatriBAP", "test-5pMXJR6YPt")@
parsePaymentOrderId :: Text -> Maybe (Text, Text)
parsePaymentOrderId txnId =
  let parts = T.splitOn "-" txnId
   in if length parts < 3
        then Nothing
        else
          let merchantId = head parts
              orderShortId = T.intercalate "-" (init (tail parts))
           in Just (merchantId, orderShortId)

-- ---------------------------------------------------------------------------
-- Offer detection
-- ---------------------------------------------------------------------------

-- | Check whether a Juspay order status response represents an offer transaction.
--
-- An offer is detected when @txn_detail.offer_deduction_amount > 0@.
isOfferTransaction :: OrderStatusResp -> Bool
isOfferTransaction resp =
  case resp.txn_detail of
    Nothing -> False
    Just txnDetail ->
      let offerDeduction = fromMaybe "0" txnDetail.offer_deduction_amount
          parsed = readMaybe @Double (T.unpack offerDeduction)
       in maybe False (> 0) parsed

-- ---------------------------------------------------------------------------
-- RRN extraction
-- ---------------------------------------------------------------------------

-- | Extract the payment RRN from the Juspay response, zero-padded to 12 characters.
extractPaymentRrn :: OrderStatusResp -> Maybe Text
extractPaymentRrn resp = do
  pgResp <- resp.payment_gateway_response
  rawRrn <- pgResp.rrn
  if T.null rawRrn
    then Nothing
    else Just $ padRrn rawRrn
  where
    padRrn rrn
      | T.length rrn < 12 = T.replicate (12 - T.length rrn) "0" <> rrn
      | otherwise = rrn

-- ---------------------------------------------------------------------------
-- Enrichment: fetch Juspay order status and apply to a PaymentSettlementReport
-- ---------------------------------------------------------------------------

-- | Enrich a 'PaymentSettlementReport' by calling the Juspay Order Status API.
--
--   1. If Juspay returns nothing -> report unchanged.
--   2. If offer -> apply offer fields (offerCode, offerId, amounts).
--   3. If merchant (subway), not offer -> recompute settlement from
--      effective_amount minus split_settlement details.
--   4. Otherwise (vendor, not offer) -> populate amounts and RRN.
enrichPaymentReport ::
  ( EncFlow m r,
    Metrics.CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SettlementServiceConfig ->
  PaymentSettlementReport ->
  m (Maybe PaymentSettlementReport)
enrichPaymentReport config report = do
  mbResp <- fetchOrderStatus config report.orderId
  case mbResp of
    Nothing -> pure (Just report)
    Just resp ->
      let offer = isOfferTransaction resp
          isMerchant = config.splitSettlementCustomerType == Just MERCHANT
       in if offer
            then pure $ Just $ applyOfferResponse resp report
            else
              if isMerchant
                then pure $ Just $ applyMerchantResponse resp report
                else pure $ Just $ applyBaseResponse resp report

-- | Vendor (non-offer): populate amounts and RRN from Juspay.
applyBaseResponse :: OrderStatusResp -> PaymentSettlementReport -> PaymentSettlementReport
applyBaseResponse resp report =
  let apiRrn = extractPaymentRrn resp
      settAmt = maybe report.settlementAmount realToFrac resp.effective_amount
   in report
        { rrn = apiRrn <|> report.rrn,
          isOffer = Just False,
          offerCode = Nothing,
          offerId = Nothing,
          actualAmount = Just (realToFrac resp.amount),
          settlementAmount = settAmt
        }

-- | Offer transaction: populate offer fields and Juspay amounts.
applyOfferResponse :: OrderStatusResp -> PaymentSettlementReport -> PaymentSettlementReport
applyOfferResponse resp report =
  let apiRrn = extractPaymentRrn resp
      offers = fromMaybe [] resp.offers
      oCode = listToMaybe offers >>= Juspay.offer_code
      oId = listToMaybe offers >>= Juspay.offer_id
      settAmt = maybe report.settlementAmount realToFrac resp.effective_amount
   in report
        { rrn = apiRrn <|> report.rrn,
          isOffer = Just True,
          offerCode = oCode,
          offerId = oId,
          actualAmount = Just (realToFrac resp.amount),
          settlementAmount = settAmt
        }

-- | Merchant (subway), not offer: recompute settlement from
-- effective_amount minus split_settlement_response details.
applyMerchantResponse :: OrderStatusResp -> PaymentSettlementReport -> PaymentSettlementReport
applyMerchantResponse resp report =
  let apiRrn = extractPaymentRrn resp
      settType = fromMaybe CREDIT report.settlementType
      multiplier :: HighPrecMoney
      multiplier = case settType of
        CREDIT -> 1.0
        DEBIT -> -1.0
      juspayAmount = realToFrac resp.amount
      computedSettlement = case resp.effective_amount of
        Nothing -> report.settlementAmount
        Just effAmt ->
          let baseAmt = realToFrac effAmt * multiplier
              splitDeduction = computeSplitTotal resp * negate multiplier
           in baseAmt + splitDeduction
   in report
        { rrn = apiRrn <|> report.rrn,
          isOffer = Just False,
          offerCode = Nothing,
          offerId = Nothing,
          actualAmount = Just juspayAmount,
          settlementAmount = computedSettlement
        }

computeSplitTotal :: OrderStatusResp -> HighPrecMoney
computeSplitTotal resp =
  case resp.split_settlement_response of
    Nothing -> 0
    Just splitResp ->
      case Juspay.split_details splitResp of
        Nothing -> 0
        Just details -> sum $ map (\d -> fromMaybe 0 (d.amount :: Maybe HighPrecMoney)) details
