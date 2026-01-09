{-
  Copyright 2022-25, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.External.Wallet.Interface
  ( module Reexport,
    module Kernel.External.Wallet.Interface,
  )
where

import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Wallet.Interface.Juspay as Juspay
import Kernel.External.Wallet.Interface.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Utils.Common

createWallet ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  Payment.PaymentServiceConfig ->
  CreateWalletReq ->
  m CreateWalletResp
createWallet config req = case config of
  Payment.JuspayConfig cfg -> Juspay.createWallet cfg req
  Payment.StripeConfig _ -> throwError (InvalidRequest "Stripe is not supported for wallet operations")

walletPosting ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  Payment.PaymentServiceConfig ->
  WalletPostingReq ->
  m WalletPostingResp
walletPosting config req = case config of
  Payment.JuspayConfig cfg -> Juspay.walletPosting cfg req
  Payment.StripeConfig _ -> throwError (InvalidRequest "Stripe is not supported for wallet operations")

walletReversal ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  Payment.PaymentServiceConfig ->
  WalletReversalReq ->
  m WalletReversalResp
walletReversal config req = case config of
  Payment.JuspayConfig cfg -> Juspay.walletReversal cfg req
  Payment.StripeConfig _ -> throwError (InvalidRequest "Stripe is not supported for wallet operations")

walletBalance ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  Payment.PaymentServiceConfig ->
  WalletBalanceReq ->
  m WalletBalanceResp
walletBalance config req = case config of
  Payment.JuspayConfig cfg -> Juspay.walletBalance cfg req
  Payment.StripeConfig _ -> throwError (InvalidRequest "Stripe is not supported for wallet operations")

walletVerifyTxn ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  Payment.PaymentServiceConfig ->
  WalletVerifyTxnReq ->
  m WalletVerifyTxnResp
walletVerifyTxn config req = case config of
  Payment.JuspayConfig cfg -> Juspay.walletVerifyTxn cfg req
  Payment.StripeConfig _ -> throwError (InvalidRequest "Stripe is not supported for wallet operations")
