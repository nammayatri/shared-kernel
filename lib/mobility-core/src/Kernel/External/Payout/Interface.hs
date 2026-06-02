{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.External.Payout.Interface
  ( module Reexport,
    module Kernel.External.Payout.Interface,
  )
where

import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payout.Interface.Juspay as Juspay
import qualified Kernel.External.Payout.Interface.Stripe as Stripe
import Kernel.External.Payout.Interface.Types as Reexport
import Kernel.External.Payout.Types as Reexport
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common

createPayoutOrder ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl]
  ) =>
  PayoutServiceConfig ->
  CreatePayoutOrderReq ->
  m CreatePayoutOrderResp
createPayoutOrder serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.createPayoutOrder cfg req
  StripeConfig cfg -> do
    connectedAccountId <- req.mConnectedAccountId & fromMaybeM (InvalidRequest "connectedAccountId required for Stripe payout")

    -- Check Fleet VA available balance and compute adjusted transfer amount if needed.
    -- If fleetAvail + transferAmount < payoutAmount, top up the transfer to cover the shortfall.
    let computeAdjustedTransfer = do
          fleetBalance <- Stripe.getBalance cfg (Just connectedAccountId)
          let fleetAvail = Stripe.getAvailableForCurrency req.currency fleetBalance
          if fleetAvail + req.transferAmount >= req.amount
            then pure (req.transferAmount, Nothing)
            else do
              let topUp = req.amount - fleetAvail - req.transferAmount
                  adjustedAmt = req.transferAmount + topUp
              -- Verify merchant (platform) VA can cover the increased transfer
              merchantBalance <- Stripe.getBalance cfg Nothing
              let merchantAvail = Stripe.getAvailableForCurrency req.currency merchantBalance
              when (merchantAvail < adjustedAmt) $
                throwError $
                  InvalidRequest $
                    "Merchant platform account has insufficient balance. Available: "
                      <> show merchantAvail
                      <> ", Required: "
                      <> show adjustedAmt
              logInfo $
                "Fleet VA balance insufficient (available: "
                  <> show fleetAvail
                  <> "). Topping up transfer by "
                  <> show topUp
                  <> " -> adjusted transfer: "
                  <> show adjustedAmt
              pure (adjustedAmt, Just topUp)

    (adjustedTransferAmount, merchantTopUpAmount) <- computeAdjustedTransfer

    createTransferResp <- Stripe.createTransfer cfg (mkTransferReq connectedAccountId adjustedTransferAmount req)
    -- In case if external payout api call failed, we still need to store transferId and transferStatus
    result <- withTryCatch "createExternalPayout" $ Stripe.createExternalPayout cfg req
    createExternalPayoutResp <- case result of
      Right resp -> pure resp
      Left e -> do
        let err = fromException @Payment.StripeError e
            errorCode = err <&> toErrorCode
            errorMessage = err >>= toMessage
        logError $ "Error while create external payout : " <> show err <> "error code : " <> show errorCode <> "error message : " <> show errorMessage <> " orderId: " <> req.orderId
        pure
          CreateExternalPayoutResp
            { orderId = req.orderId,
              status = FAILURE,
              orderType = Just req.orderType,
              idAssignedByServiceProvider = Nothing,
              amount = req.amount,
              customerId = Just req.customerId
            }
    pure $ mkCreatePayoutOrderResp merchantTopUpAmount createTransferResp createExternalPayoutResp
  where
    mkTransferReq :: Text -> HighPrecMoney -> CreatePayoutOrderReq -> CreateTransferReq
    mkTransferReq connectedAccountId adjustedTransferAmount CreatePayoutOrderReq {..} =
      CreateTransferReq
        { amount = adjustedTransferAmount,
          currency,
          senderAccountId = TransferPlatformAccount,
          destinationAccount = TransferConnectedAccount connectedAccountId,
          description = Just remark
        }

    mkCreatePayoutOrderResp :: Maybe HighPrecMoney -> CreateTransferResp -> CreateExternalPayoutResp -> CreatePayoutOrderResp
    mkCreatePayoutOrderResp merchantTopUpAmount CreateTransferResp {transferId, transferStatus} CreateExternalPayoutResp {..} =
      CreatePayoutOrderResp
        { orderId,
          status,
          transferStatus = Just transferStatus,
          orderType,
          transferId = Just transferId,
          idAssignedByServiceProvider,
          udf1 = Nothing,
          udf2 = Nothing,
          udf3 = Nothing,
          udf4 = Nothing,
          udf5 = Nothing,
          amount,
          refunds = Nothing,
          payments = Nothing,
          fulfillments = Nothing,
          customerId,
          merchantTopUpAmount
        }

payoutOrderStatus ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PayoutServiceConfig ->
  PayoutOrderStatusReq ->
  m PayoutOrderStatusResp
payoutOrderStatus serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.payoutOrderStatus cfg req
  StripeConfig cfg -> do
    resp <- Stripe.externalPayoutOrderStatus cfg req
    pure $ mkPayoutOrderStatusResp resp
  where
    mkPayoutOrderStatusResp :: ExternalPayoutOrderStatusResp -> PayoutOrderStatusResp
    mkPayoutOrderStatusResp CreateExternalPayoutResp {..} =
      CreatePayoutOrderResp
        { orderId,
          status,
          transferStatus = req.transferStatus,
          orderType,
          transferId = req.transferId,
          idAssignedByServiceProvider,
          udf1 = Nothing,
          udf2 = Nothing,
          udf3 = Nothing,
          udf4 = Nothing,
          udf5 = Nothing,
          amount,
          refunds = Nothing,
          payments = Nothing,
          fulfillments = Nothing,
          customerId,
          merchantTopUpAmount = Nothing
        }

createTransfer ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  PayoutServiceConfig ->
  CreateTransferReq ->
  m CreateTransferResp
createTransfer config req = case config of
  JuspayConfig _ -> throwError $ InternalError "Juspay Create Transfer not supported."
  StripeConfig cfg -> Stripe.createTransfer cfg req
