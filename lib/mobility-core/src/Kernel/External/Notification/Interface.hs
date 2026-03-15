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

import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import EulerHS.Prelude
import Kernel.External.Notification.FCM.Types (LiveActivityReq)
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
import Kernel.Utils.Servant.Client

notifyPerson ::
  ( MonadFlow m,
    EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    ToJSON a,
    ToJSON b,
    HasRequestId r,
    MonadReader r m
  ) =>
  NotificationServiceConfig ->
  NotificationReq a b ->
  Maybe LiveActivityReq ->
  m () ->
  m ()
notifyPerson serviceConfig req liveAcitvityRequest action = do
  notificationId <- generateGUID

  case serviceConfig of
    FCMConfig cfg -> FCM.notifyPerson cfg req liveAcitvityRequest action (Just notificationId) EulerHS.Prelude.id
    PayTMConfig cfg -> PayTM.notifyPerson cfg req
    GRPCConfig _ -> throwError $ InternalError "GRPC notification type not supported."

-- | Send notifications to a batch of recipients with bounded concurrency.
-- At most @maxConcurrency@ (default 10) notifications are in-flight at once,
-- preventing a large fan-out from overwhelming FCM or saturating local resources.
notifyPersonBatch ::
  ( MonadFlow m,
    EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    ToJSON a,
    ToJSON b,
    HasRequestId r,
    MonadReader r m
  ) =>
  NotificationServiceConfig ->
  Int -> -- max concurrency (e.g. 10)
  [(NotificationReq a b, Maybe LiveActivityReq, m ())] -> -- (request, liveActivity, tokenCleanup action)
  m ()
notifyPersonBatch serviceConfig maxConcurrency items = do
  sem <- liftIO $ newQSem maxConcurrency
  forM_ items $ \(req, liveActivityReq, action) ->
    fork ("notifyPersonBatch " <> req.auth.recipientId) $ do
      liftIO $ waitQSem sem
      finally
        (notifyPerson serviceConfig req liveActivityReq action)
        (liftIO $ signalQSem sem)
  where
    finally :: (MonadCatch m) => m a -> m () -> m a
    finally act cleanup = do
      result <- try act
      cleanup
      case result of
        Right val -> pure val
        Left (e :: SomeException) -> throwM e

notifyPersonWithAllProviders ::
  ( EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    ToJSON a,
    ToJSON b,
    ToJSON c,
    HasRequestId r,
    MonadReader r m,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  NotficationServiceHandler m a c ->
  NotificationReq a b ->
  Maybe LiveActivityReq ->
  m () ->
  m ()
notifyPersonWithAllProviders NotficationServiceHandler {..} req liveAcitvityRequest action = do
  serviceList <- getNotificationServiceList
  notificationId <- generateGUID
  callNotifPerson serviceList notificationId
  where
    callNotifPerson [] _notificationId = return ()
    callNotifPerson (serviceProvider : remaining) notificationId = do
      fork ("notifying person with following service " <> show serviceProvider <> "for following notification id : " <> notificationId) $ do
        serviceProviderConfig <- getServiceConfig serviceProvider
        case serviceProviderConfig of
          FCMConfig cfg -> FCM.notifyPerson cfg req liveAcitvityRequest action (Just notificationId) iosModifier
          PayTMConfig cfg -> PayTM.notifyPerson cfg req
          GRPCConfig cfg -> GRPC.notifyPerson cfg req notificationId
      callNotifPerson remaining notificationId
