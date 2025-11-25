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

import qualified Kernel.External.Wallet.Interface.Juspay as Juspay
import Kernel.External.Wallet.Interface.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

createWallet ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  WalletServiceConfig ->
  CreateWalletReq ->
  m CreateWalletResp
createWallet config req = case config of
  JuspayWalletConfig cfg -> Juspay.createWallet cfg req

walletPosting ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  WalletServiceConfig ->
  WalletPostingReq ->
  m WalletPostingResp
walletPosting config req = case config of
  JuspayWalletConfig cfg -> Juspay.walletPosting cfg req

walletReversal ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  WalletServiceConfig ->
  WalletReversalReq ->
  m WalletReversalResp
walletReversal config req = case config of
  JuspayWalletConfig cfg -> Juspay.walletReversal cfg req

walletBalance ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  WalletServiceConfig ->
  WalletBalanceReq ->
  m WalletBalanceResp
walletBalance config req = case config of
  JuspayWalletConfig cfg -> Juspay.walletBalance cfg req

walletVerifyTxn ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  WalletServiceConfig ->
  WalletVerifyTxnReq ->
  m WalletVerifyTxnResp
walletVerifyTxn config req = case config of
  JuspayWalletConfig cfg -> Juspay.walletVerifyTxn cfg req
