{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Notification.Interface.GRPC where

import EulerHS.Prelude
import qualified Kernel.External.Notification.GRPC.Flow as GRPC
import qualified Kernel.External.Notification.GRPC.Types as GRPC
import qualified Kernel.External.Notification.Interface.Types as Interface
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Utils.Time

notifyPerson ::
  ( MonadFlow m,
    ToJSON a,
    ToJSON b,
    Redis.HedisFlow m r
  ) =>
  GRPC.GRPCConfig ->
  Interface.NotificationReq a b ->
  Text ->
  m ()
notifyPerson config req notificationId = do
  defaultTtlTime <- addUTCTime (secondsToNominalDiffTime config.defaultTtl) <$> getCurrentTime -- this time ? UTC or IST ?
  let title = GRPC.GRPCNotificationTitle req.title
      body = GRPC.GRPCNotificationBody req.body
      notificationType = show req.category
      notificationData =
        GRPC.GrpcNotificationData
          { entityId = req.entity.entityIds,
            entityType = show req.entity.entityType,
            entityData = req.entity.entityData,
            category = notificationType,
            showNotification = show req.showNotification,
            ttl = fromMaybe defaultTtlTime req.ttl,
            streamName = req.auth.recipientId,
            ..
          }
  GRPC.notifyPerson config notificationData
