{-
  Copyright 2022-25, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Wallet.Juspay.Flow where

import EulerHS.Types as Euler
import Kernel.External.Wallet.Interface.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (callAPI, fromEitherM)
import Kernel.Utils.Servant.Client (HasRequestId)
import Servant

type CreateWalletAPI =
  "wallet"
    :> "customer"
    :> Header "issuer" Text
    :> Header "x-api-key" Text
    :> ReqBody '[JSON] CreateWalletReq
    :> Post '[JSON] CreateWalletResp

createWallet ::
  (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) =>
  BaseUrl ->
  Text ->
  Text ->
  CreateWalletReq ->
  m CreateWalletResp
createWallet url apiKey issuer req = do
  let proxy = Proxy @CreateWalletAPI
      eulerClient = Euler.client proxy (Just issuer) (Just apiKey) req
  callAPI url eulerClient "create-wallet" proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call create wallet API: " <> show err)

type WalletPostingAPI =
  "wallet"
    :> "reward"
    :> Header "issuer" Text
    :> Header "x-api-key" Text
    :> Header "version" Text
    :> ReqBody '[JSON] WalletPostingReq
    :> Post '[JSON] WalletPostingResp

walletPosting ::
  (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) =>
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  WalletPostingReq ->
  m WalletPostingResp
walletPosting url apiKey issuer mVersion req = do
  let proxy = Proxy @WalletPostingAPI
      eulerClient = Euler.client proxy (Just issuer) (Just apiKey) mVersion req
  callAPI url eulerClient "wallet-posting" proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call wallet posting API: " <> show err)

type WalletReversalAPI =
  "wallet"
    :> "reward"
    :> "reversal"
    :> Header "issuer" Text
    :> Header "x-api-key" Text
    :> ReqBody '[JSON] WalletReversalReq
    :> Post '[JSON] WalletReversalResp

walletReversal ::
  (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) =>
  BaseUrl ->
  Text ->
  Text ->
  WalletReversalReq ->
  m WalletReversalResp
walletReversal url apiKey issuer req = do
  let proxy = Proxy @WalletReversalAPI
      eulerClient = Euler.client proxy (Just issuer) (Just apiKey) req
  callAPI url eulerClient "wallet-reversal" proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call wallet reversal API: " <> show err)

type WalletBalanceAPI =
  "wallet"
    :> "balance"
    :> QueryParam' '[Required, Strict] "customerId" Text
    :> QueryParam "requireHistory" Bool
    :> Header "issuer" Text
    :> Header "x-api-key" Text
    :> Get '[JSON] WalletBalanceResp

walletBalance ::
  (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) =>
  BaseUrl ->
  Text ->
  Text ->
  WalletBalanceReq ->
  m WalletBalanceResp
walletBalance url apiKey issuer WalletBalanceReq {..} = do
  let proxy = Proxy @WalletBalanceAPI
      eulerClient = Euler.client proxy customerId requireHistory (Just issuer) (Just apiKey)
  callAPI url eulerClient "wallet-balance" proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call wallet balance API: " <> show err)

type WalletVerifyTxnAPI =
  "wallet"
    :> "reward"
    :> QueryParam' '[Required, Strict] "operationId" Text
    :> Header "issuer" Text
    :> Header "x-api-key" Text
    :> Get '[JSON] WalletVerifyTxnResp

walletVerifyTxn ::
  (Metrics.CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) =>
  BaseUrl ->
  Text ->
  Text ->
  WalletVerifyTxnReq ->
  m WalletVerifyTxnResp
walletVerifyTxn url apiKey issuer WalletVerifyTxnReq {..} = do
  let proxy = Proxy @WalletVerifyTxnAPI
      eulerClient = Euler.client proxy operationId (Just issuer) (Just apiKey)
  callAPI url eulerClient "wallet-verify-txn" proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call wallet verify txn API: " <> show err)
