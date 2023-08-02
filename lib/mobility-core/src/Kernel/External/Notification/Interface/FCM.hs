module Kernel.External.Notification.Interface.FCM where

import Data.Default.Class
import qualified Kernel.External.Notification.FCM.Flow as FCM
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Notification.Interface.Types as Interface
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

notifyPerson ::
  ( CoreMetrics m,
    MonadFlow m,
    Redis.HedisFlow m r,
    Default a,
    ToJSON a,
    ToJSON b
  ) =>
  FCM.FCMConfig ->
  Interface.NotificationReq a b ->
  Bool ->
  m ()
notifyPerson config req isMutable = do
  let title = FCM.FCMNotificationTitle req.title
      body = FCM.FCMNotificationBody req.body
      notificationType = interfaceCategoryToFCMNotificationType req.category
      notificationData =
        FCM.FCMData
          { fcmNotificationType = notificationType,
            fcmShowNotification = interfaceShowNotificationToFCMShowNotification req.showNotification,
            fcmEntityType = interfaceEntityTypeToFCMEntityType req.entity.entityType,
            fcmEntityIds = req.entity.entityIds,
            fcmEntityData = req.entity.entityData,
            fcmNotificationJSON = FCM.createAndroidNotification title body notificationType,
            fcmOverlayNotificationJSON = Nothing
          }
  FCM.notifyPersonWithPriority
    config
    (interfaceMessagePriorityToFCMMessagePriority <$> req.messagePriority)
    isMutable
    notificationData
    (FCM.FCMNotificationRecipient req.auth.recipientId (FCM.FCMRecipientToken <$> req.auth.fcmToken))

interfaceMessagePriorityToFCMMessagePriority :: Interface.MessagePriority -> FCM.FCMAndroidMessagePriority
interfaceMessagePriorityToFCMMessagePriority = \case
  Interface.NORMAL -> FCM.NORMAL
  Interface.HIGH -> FCM.HIGH

interfaceEntityTypeToFCMEntityType :: Interface.EntityType -> FCM.FCMEntityType
interfaceEntityTypeToFCMEntityType = \case
  Interface.SearchRequest -> FCM.SearchRequest
  Interface.Product -> FCM.Product
  Interface.Merchant -> FCM.Merchant
  Interface.Person -> FCM.Person

interfaceCategoryToFCMNotificationType :: Interface.Category -> FCM.FCMNotificationType
interfaceCategoryToFCMNotificationType = \case
  Interface.REGISTRATION_APPROVED -> FCM.REGISTRATION_APPROVED
  Interface.EXPIRED_CASE -> FCM.EXPIRED_CASE
  Interface.CANCELLED_PRODUCT -> FCM.CANCELLED_PRODUCT
  Interface.REALLOCATE_PRODUCT -> FCM.REALLOCATE_PRODUCT
  Interface.DRIVER_ASSIGNMENT -> FCM.DRIVER_ASSIGNMENT
  Interface.TRIP_STARTED -> FCM.TRIP_STARTED
  Interface.TRIP_FINISHED -> FCM.TRIP_FINISHED
  Interface.ALLOCATION_REQUEST -> FCM.ALLOCATION_REQUEST
  Interface.ALLOCATION_REQUEST_UNASSIGNED -> FCM.ALLOCATION_REQUEST_UNASSIGNED
  Interface.ACCOUNT_DISABLED -> FCM.ACCOUNT_DISABLED
  Interface.TRIGGER_SERVICE -> FCM.TRIGGER_SERVICE
  Interface.FARE_POLICY_CHANGED -> FCM.FARE_POLICY_CHANGED
  Interface.DISCOUNT_CHANGED -> FCM.DISCOUNT_CHANGED
  Interface.QUOTE_RECEIVED -> FCM.QUOTE_RECEIVED
  Interface.NEW_RIDE_AVAILABLE -> FCM.NEW_RIDE_AVAILABLE
  Interface.DRIVER_QUOTE_INCOMING -> FCM.DRIVER_QUOTE_INCOMING
  Interface.DRIVER_ON_THE_WAY -> FCM.DRIVER_ON_THE_WAY
  Interface.DRIVER_HAS_REACHED -> FCM.DRIVER_HAS_REACHED
  Interface.CLEARED_FARE -> FCM.CLEARED_FARE
  Interface.CANCELLED_SEARCH_REQUEST -> FCM.CANCELLED_SEARCH_REQUEST
  Interface.NEW_MESSAGE -> FCM.NEW_MESSAGE
  Interface.REFERRAL_ACTIVATED -> FCM.REFERRAL_ACTIVATED
  Interface.CHAT_MESSAGE -> FCM.CHAT_MESSAGE
  Interface.SAFETY_ALERT -> FCM.SAFETY_ALERT

interfaceShowNotificationToFCMShowNotification :: Interface.ShowNotification -> FCM.FCMShowNotification
interfaceShowNotificationToFCMShowNotification = \case
  Interface.SHOW -> FCM.SHOW
  Interface.DO_NOT_SHOW -> FCM.DO_NOT_SHOW
