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

import qualified Kernel.External.Payout.Interface.Juspay as Juspay
import qualified Kernel.External.Payout.Interface.Stripe as Stripe
import Kernel.External.Payout.Interface.Types as Reexport
import Kernel.External.Payout.Types as Reexport
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (GenericError (InvalidRequest))
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
  StripeConfig cfg -> Stripe.createPayoutOrder cfg req

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
  StripeConfig cfg -> Stripe.payoutOrderStatus cfg req

listExternalAccounts ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PayoutServiceConfig ->
  ListExternalAccountsReq ->
  m ListExternalAccountsResp
listExternalAccounts serviceConfig req = case serviceConfig of
  JuspayConfig _ -> throwError $ InvalidRequest "Juspay List External Accounts not supported."
  StripeConfig cfg -> Stripe.listExternalAccounts cfg req

createExternalAccount ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PayoutServiceConfig ->
  CreateExternalAccountReq ->
  m CreateExternalAccountResp
createExternalAccount serviceConfig req = case serviceConfig of
  JuspayConfig _ -> throwError $ InvalidRequest "Juspay Create External Account not supported."
  StripeConfig cfg -> Stripe.createExternalAccount cfg req

getExternalAccount ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PayoutServiceConfig ->
  GetExternalAccountReq ->
  m GetExternalAccountResp
getExternalAccount serviceConfig req = case serviceConfig of
  JuspayConfig _ -> throwError $ InvalidRequest "Juspay Get External Account not supported."
  StripeConfig cfg -> Stripe.getExternalAccount cfg req

updateExternalAccount ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PayoutServiceConfig ->
  UpdateExternalAccountReq ->
  m UpdateExternalAccountResp
updateExternalAccount serviceConfig req = case serviceConfig of
  JuspayConfig _ -> throwError $ InvalidRequest "Juspay Update External Account not supported"
  StripeConfig cfg -> Stripe.updateExternalAccount cfg req

deleteExternalAccount ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PayoutServiceConfig ->
  DeleteExternalAccountReq ->
  m DeleteExternalAccountResp
deleteExternalAccount serviceConfig req = case serviceConfig of
  JuspayConfig _ -> throwError $ InvalidRequest "Juspay Delete External Account not supported."
  StripeConfig cfg -> Stripe.deleteExternalAccount cfg req
