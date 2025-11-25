{-
  Copyright 2022-25, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Wallet.Interface.Juspay
  ( module Reexport,
    createWallet,
    walletPosting,
    walletReversal,
    walletBalance,
    walletVerifyTxn,
  )
where

import Kernel.External.Encryption
import Kernel.External.Wallet.Interface.Types
import Kernel.External.Wallet.Juspay.Config as Reexport
import qualified Kernel.External.Wallet.Juspay.Flow as Juspay
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Servant.Client (HasRequestId)

createWallet ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r
  ) =>
  JuspayWalletConfig ->
  CreateWalletReq ->
  m CreateWalletResp
createWallet config req = do
  apiKey <- decrypt config.apiKey
  Juspay.createWallet config.url apiKey config.issuer req

walletPosting ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r
  ) =>
  JuspayWalletConfig ->
  WalletPostingReq ->
  m WalletPostingResp
walletPosting config req = do
  apiKey <- decrypt config.apiKey
  Juspay.walletPosting config.url apiKey config.issuer (Just config.rewardApiVersion) req

walletReversal ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r
  ) =>
  JuspayWalletConfig ->
  WalletReversalReq ->
  m WalletReversalResp
walletReversal config req = do
  apiKey <- decrypt config.apiKey
  Juspay.walletReversal config.url apiKey config.issuer req

walletBalance ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r
  ) =>
  JuspayWalletConfig ->
  WalletBalanceReq ->
  m WalletBalanceResp
walletBalance config req = do
  apiKey <- decrypt config.apiKey
  Juspay.walletBalance config.url apiKey config.issuer req

walletVerifyTxn ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r
  ) =>
  JuspayWalletConfig ->
  WalletVerifyTxnReq ->
  m WalletVerifyTxnResp
walletVerifyTxn config req = do
  apiKey <- decrypt config.apiKey
  Juspay.walletVerifyTxn config.url apiKey config.issuer req
