{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.External.Notification.Interface
  ( module Reexport,
    module Kernel.External.Notification.Interface,
  )
where

import EulerHS.Prelude
import qualified Kernel.External.Notification.Interface.FCM as FCM
import qualified Kernel.External.Notification.Interface.GRPC as GRPC
import qualified Kernel.External.Notification.Interface.PayTM as PayTM
import Kernel.External.Notification.Interface.Types as Reexport
import Kernel.External.Notification.Types as Reexport
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Field
import Kernel.Utils.Error.Throwing (throwError)

notifyPerson' ::
  ( MonadFlow m,
    EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    ToJSON a,
    ToJSON b
  ) =>
  NotificationServiceConfig ->
  NotificationReq a b ->
  m () ->
  Bool ->
  m ()
notifyPerson' serviceConfig req action isMutable = do
  notificationId <- generateGUID
  case serviceConfig of
    FCMConfig cfg -> FCM.notifyPerson cfg req action isMutable (Just notificationId) EulerHS.Prelude.id
    PayTMConfig cfg -> PayTM.notifyPerson cfg req
    GRPCConfig _ -> throwError $ InternalError "GRPC notification type not supported."

notifyPersonWithAllProviders ::
  ( EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    ToJSON a,
    ToJSON b,
    ToJSON c,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  NotficationServiceHandler m a c ->
  NotificationReq a b ->
  m () ->
  Bool ->
  m ()
notifyPersonWithAllProviders NotficationServiceHandler {..} req action isMutable = do
  serviceList <- getNotificationServiceList
  notificationId <- generateGUID
  callNotifPerson serviceList notificationId
  where
    callNotifPerson [] _notificationId = return ()
    callNotifPerson (serviceProvider : remaining) notificationId = do
      fork ("notifying person with following service " <> show serviceProvider <> "for following notification id : " <> notificationId) $ do
        serviceProviderConfig <- getServiceConfig serviceProvider
        case serviceProviderConfig of
          FCMConfig cfg -> FCM.notifyPerson cfg req action isMutable (Just notificationId) iosModifier
          PayTMConfig cfg -> PayTM.notifyPerson cfg req
          GRPCConfig cfg -> GRPC.notifyPerson cfg req notificationId
      callNotifPerson remaining notificationId

notifyPerson ::
  ( MonadFlow m,
    EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    ToJSON a,
    ToJSON b
  ) =>
  NotificationServiceConfig ->
  NotificationReq a b ->
  m () ->
  m ()
notifyPerson serviceConfig req action = notifyPerson' serviceConfig req action False

notifyPersonWithMutableContent ::
  ( MonadFlow m,
    EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    ToJSON a,
    ToJSON b
  ) =>
  NotificationServiceConfig ->
  NotificationReq a b ->
  m () ->
  m ()
notifyPersonWithMutableContent serviceConfig req action = notifyPerson' serviceConfig req action True
