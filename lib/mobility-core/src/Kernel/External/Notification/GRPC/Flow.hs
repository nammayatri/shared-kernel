{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Notification.GRPC.Flow where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UU
import EulerHS.Prelude
import Kernel.External.Notification.GRPC.Types
import Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Common
import Kernel.Utils.Common hiding (Error)

notifyPerson ::
  ( MonadFlow m,
    Redis.HedisFlow m r,
    ToJSON a,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  GRPCConfig ->
  GrpcNotificationData a ->
  m ()
notifyPerson cfg notificationData = do
  now <- getCurrentTime
  maxShards <- asks (.maxNotificationShards)
  let idToShardNumber uuidTxt = fromIntegral ((\(a, b) -> a + b) (UU.toWords64 uuidTxt)) `mod` (fromIntegral maxShards :: Integer)
      shardId :: Integer = idToShardNumber . fromJust $ UU.fromText notificationData.streamId
  let notificationStreamId =
        case T.splitOn "-" notificationData.streamId of
          [startUuid, midOneUuid, _, _] -> T.intercalate "-" [startUuid, midOneUuid]
          _ -> notificationData.streamId
  _ <- Hedis.withCrossAppRedis $ Hedis.publish "active-notification" notificationStreamId
  void $ Hedis.withCrossAppRedis $ Hedis.xAddExp ("N" <> notificationStreamId <> "{" <> (show shardId) <> "}") "*" (buildFieldValue notificationData now) cfg.streamExpirationTime
  where
    buildFieldValue notifData createdAt =
      [ ("entity.id", TE.encodeUtf8 notifData.entityId),
        ("entity.type", TE.encodeUtf8 $ notifData.entityType),
        ("entity.data", TE.encodeUtf8 $ encodeToText notifData.entityData),
        ("category", TE.encodeUtf8 $ notifData.category),
        ("title", TE.encodeUtf8 notifData.title.getGRPCNotificationTitle),
        ("body", TE.encodeUtf8 notifData.body.getGRPCNotificationBody),
        ("show", TE.encodeUtf8 $ notifData.showNotification),
        ("ttl", TE.encodeUtf8 $ show notifData.ttl),
        ("created_at", TE.encodeUtf8 $ show createdAt),
        ("id", TE.encodeUtf8 $ notificationData.notificationId)
      ]
