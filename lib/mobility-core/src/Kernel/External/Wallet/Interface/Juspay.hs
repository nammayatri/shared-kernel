{-
  Copyright 2022-25, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Wallet.Interface.Juspay
  ( createWallet,
    walletPosting,
    walletReversal,
    walletBalance,
    walletVerifyTxn,
  )
where

import Kernel.External.Encryption
import qualified Kernel.External.Payment.Juspay.Config as PaymentJuspay
import Kernel.External.Wallet.Interface.Types
import qualified Kernel.External.Wallet.Juspay.Flow as Juspay
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Utils.Error.Throwing (fromMaybeM)
import Kernel.Utils.Servant.Client (HasRequestId)

createWallet ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r
  ) =>
  PaymentJuspay.JuspayCfg ->
  CreateWalletReq ->
  m CreateWalletResp
createWallet config req = do
  issuer <- fromMaybeM (InvalidRequest "walletIssuer is required in Juspay config") config.walletIssuer
  apiKey <- decrypt config.apiKey
  Juspay.createWallet config.url apiKey issuer req

walletPosting ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r
  ) =>
  PaymentJuspay.JuspayCfg ->
  WalletPostingReq ->
  m WalletPostingResp
walletPosting config req = do
  issuer <- fromMaybeM (InvalidRequest "walletIssuer is required in Juspay config") config.walletIssuer
  rewardApiVersion <- fromMaybeM (InvalidRequest "walletRewardApiVersion is required in Juspay config") config.walletRewardApiVersion
  apiKey <- decrypt config.apiKey
  Juspay.walletPosting config.url apiKey issuer (Just rewardApiVersion) req

walletReversal ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r
  ) =>
  PaymentJuspay.JuspayCfg ->
  WalletReversalReq ->
  m WalletReversalResp
walletReversal config req = do
  issuer <- fromMaybeM (InvalidRequest "walletIssuer is required in Juspay config") config.walletIssuer
  apiKey <- decrypt config.apiKey
  Juspay.walletReversal config.url apiKey issuer req

walletBalance ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r
  ) =>
  PaymentJuspay.JuspayCfg ->
  WalletBalanceReq ->
  m WalletBalanceResp
walletBalance config req = do
  issuer <- fromMaybeM (InvalidRequest "walletIssuer is required in Juspay config") config.walletIssuer
  apiKey <- decrypt config.apiKey
  Juspay.walletBalance config.url apiKey issuer req

walletVerifyTxn ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r
  ) =>
  PaymentJuspay.JuspayCfg ->
  WalletVerifyTxnReq ->
  m WalletVerifyTxnResp
walletVerifyTxn config req = do
  issuer <- fromMaybeM (InvalidRequest "walletIssuer is required in Juspay config") config.walletIssuer
  apiKey <- decrypt config.apiKey
  Juspay.walletVerifyTxn config.url apiKey issuer req
