module Kernel.External.Notification.Interface.FCM where

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
    ToJSON a,
    ToJSON b
  ) =>
  FCM.FCMConfig ->
  Interface.NotificationReq a b ->
  Bool ->
  Maybe Text ->
  m ()
notifyPerson config req isMutable mbNotificationId = do
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
            fcmNotificationJSON = FCM.createAndroidNotification title body notificationType req.sound,
            fcmOverlayNotificationJSON = Nothing,
            fcmNotificationId = mbNotificationId
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
  Interface.EditLocation -> FCM.EditLocation

interfaceCategoryToFCMNotificationType :: Interface.Category -> FCM.FCMNotificationType
interfaceCategoryToFCMNotificationType = \case
  Interface.REGISTRATION_APPROVED -> FCM.REGISTRATION_APPROVED
  Interface.EXPIRED_CASE -> FCM.EXPIRED_CASE
  Interface.CANCELLED_PRODUCT -> FCM.CANCELLED_PRODUCT
  Interface.CANCELLED_PRODUCT_DRIVER -> FCM.CANCELLED_PRODUCT_DRIVER
  Interface.CANCELLED_PRODUCT_USER -> FCM.CANCELLED_PRODUCT_USER
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
  Interface.SAFETY_ALERT_DEVIATION -> FCM.SAFETY_ALERT_DEVIATION
  Interface.DRIVER_BIRTHDAY -> FCM.DRIVER_BIRTHDAY
  Interface.EDIT_LOCATION -> FCM.EDIT_LOCATION
  Interface.ADD_STOP -> FCM.ADD_STOP
  Interface.EDIT_STOP -> FCM.EDIT_STOP
  Interface.STOP_REACHED -> FCM.STOP_REACHED
  Interface.COINS_SUCCESS -> FCM.COINS_SUCCESS
  Interface.FOLLOW_RIDE -> FCM.FOLLOW_RIDE
  Interface.SHARE_RIDE -> FCM.SHARE_RIDE
  Interface.SOS_TRIGGERED -> FCM.SOS_TRIGGERED
  Interface.SOS_MOCK_DRILL -> FCM.SOS_MOCK_DRILL
  Interface.SOS_RESOLVED -> FCM.SOS_RESOLVED
  Interface.EMERGENCY_CONTACT_ADDED -> FCM.EMERGENCY_CONTACT_ADDED
  Interface.SOS_MOCK_DRILL_NOTIFY -> FCM.SOS_MOCK_DRILL_NOTIFY

interfaceShowNotificationToFCMShowNotification :: Interface.ShowNotification -> FCM.FCMShowNotification
interfaceShowNotificationToFCMShowNotification = \case
  Interface.SHOW -> FCM.SHOW
  Interface.DO_NOT_SHOW -> FCM.DO_NOT_SHOW
