{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.External.Payment.Interface
  ( module Reexport,
    module Kernel.External.Payment.Interface,
  )
where

import qualified Kernel.External.Payment.Interface.Juspay as Juspay
import Kernel.External.Payment.Interface.Types as Reexport
import Kernel.External.Payment.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

createOrder ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  CreateOrderReq ->
  m CreateOrderResp
createOrder serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.createOrder cfg req

orderStatus ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  OrderStatusReq ->
  m OrderStatusResp
orderStatus serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.orderStatus cfg req

offerList ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  OfferListReq ->
  m OfferListResp
offerList serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.offerList cfg req

offerApply ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  OfferApplyReq ->
  m OfferApplyResp
offerApply serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.offerApply cfg req

offerNotify ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  OfferNotifyReq ->
  m OfferNotifyResp
offerNotify serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.offerNotify cfg req

mandateRevoke ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  MandateRevokeReq ->
  m MandateRevokeRes
mandateRevoke serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.mandateRevoke cfg req

mandateNotification ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  MandateNotificationReq ->
  m MandateNotificationRes
mandateNotification serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.mandateNotification cfg req

mandateNotificationStatus ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  NotificationStatusReq ->
  m NotificationStatusResp
mandateNotificationStatus serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.mandateNotificationStatus cfg req

mandateExecution ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  PaymentServiceConfig ->
  MandateExecutionReq ->
  m MandateExecutionRes
mandateExecution serviceConfig req = case serviceConfig of
  JuspayConfig cfg -> Juspay.mandateExecution cfg req
