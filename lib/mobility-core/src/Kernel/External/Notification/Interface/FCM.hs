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
    ToJSON b,
    ToJSON c
  ) =>
  FCM.FCMConfig ->
  Interface.NotificationReq a b ->
  Maybe FCM.LiveActivityReq ->
  m () ->
  Maybe Text ->
  (FCM.FCMData a -> FCM.FCMData c) ->
  m ()
notifyPerson config req liveAcitvityRequest action mbNotificationId iosModifier = do
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
      apnsData = liveAcitvityRequest
  case apnsData of
    (Just reqLive) -> do FCM.updateLiveActivity config (FCM.FCMNotificationRecipient req.auth.recipientId (FCM.FCMRecipientToken <$> req.auth.fcmToken)) reqLive
    _ -> pure ()
  FCM.notifyPersonWithPriority
    config
    (interfaceMessagePriorityToFCMMessagePriority <$> req.messagePriority)
    action
    notificationData
    (FCM.FCMNotificationRecipient req.auth.recipientId (FCM.FCMRecipientToken <$> req.auth.fcmToken))
    iosModifier

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
  Interface.DRIVER_REACHING -> FCM.DRIVER_REACHING
  Interface.CLEARED_FARE -> FCM.CLEARED_FARE
  Interface.CANCELLED_SEARCH_REQUEST -> FCM.CANCELLED_SEARCH_REQUEST
  Interface.NEW_MESSAGE -> FCM.NEW_MESSAGE
  Interface.REFERRAL_ACTIVATED -> FCM.REFERRAL_ACTIVATED
  Interface.CHAT_MESSAGE -> FCM.CHAT_MESSAGE
  Interface.PAYMENT_PENDING -> FCM.PAYMENT_PENDING
  Interface.PAYMENT_OVERDUE -> FCM.PAYMENT_OVERDUE
  Interface.PAYMENT_FAILED -> FCM.PAYMENT_FAILED
  Interface.PAYMENT_SUCCESS -> FCM.PAYMENT_SUCCESS
  Interface.PAYMENT_MODE_MANUAL -> FCM.PAYMENT_MODE_MANUAL
  Interface.PAYMENT_NUDGE -> FCM.PAYMENT_NUDGE
  Interface.DRIVER_NOTIFY -> FCM.DRIVER_NOTIFY
  Interface.DRIVER_NOTIFY_LOCATION_UPDATE -> FCM.DRIVER_NOTIFY_LOCATION_UPDATE
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
  Interface.DOCUMENT_INVALID -> FCM.DOCUMENT_INVALID
  Interface.SCHEDULED_RIDE_NOTIFICATION -> FCM.SCHEDULED_RIDE_NOTIFICATION
  Interface.FIRST_RIDE_EVENT -> FCM.FIRST_RIDE_EVENT
  Interface.TOLL_CROSSED -> FCM.TOLL_CROSSED
  Interface.TRIP_UPDATED -> FCM.TRIP_UPDATED
  Interface.FCM_CHAT_MESSAGE -> FCM.FCM_CHAT_MESSAGE
  Interface.PAYOUT_REWARD -> FCM.PAYOUT_REWARD
  Interface.PAYOUT_VPA_REMINDER -> FCM.PAYOUT_VPA_REMINDER
  Interface.TRIGGER_FCM -> FCM.TRIGGER_FCM
  Interface.POST_RIDE_SOS_ALERT -> FCM.POST_RIDE_SOS_ALERT
  Interface.REFERRAL_FLOW -> FCM.REFERRAL_FLOW
  Interface.FILE_UPLOADED -> FCM.FILE_UPLOADED
  Interface.SCHEDULED_RIDE_REMINDER -> FCM.SCHEDULED_RIDE_REMINDER
  Interface.DRIVER_HAS_REACHED_DESTINATION -> FCM.DRIVER_HAS_REACHED_DESTINATION
  Interface.CANCELLATION_RATE_NUDGE_DAILY -> FCM.CANCELLATION_RATE_NUDGE_DAILY
  Interface.CANCELLATION_RATE_NUDGE_WEEKLY -> FCM.CANCELLATION_RATE_NUDGE_WEEKLY
  Interface.USER_FAVOURITE_DRIVER -> FCM.USER_FAVOURITE_DRIVER
  Interface.DRIVER_UNBLOCKED -> FCM.DRIVER_UNBLOCKED
  Interface.POST_RIDE_SAFETY_CHECK -> FCM.POST_RIDE_SAFETY_CHECK
  Interface.DRIVER_STOP_DETECTED -> FCM.DRIVER_STOP_DETECTED
  Interface.TO_METRO_COINS -> FCM.TO_METRO_COINS
  Interface.FROM_METRO_COINS -> FCM.FROM_METRO_COINS
  Interface.ISSUE_BREACH_EXTRA_FARE_MITIGATION -> FCM.ISSUE_BREACH_EXTRA_FARE_MITIGATION
  Interface.MARKETING_EVENTS -> FCM.MARKETING_EVENTS
  Interface.DRIVER_REQUEST_REJECTED -> FCM.DRIVER_REQUEST_REJECTED
  Interface.WMB_TRIP_ASSIGNED -> FCM.WMB_TRIP_ASSIGNED
  Interface.WMB_TRIP_STARTED -> FCM.WMB_TRIP_STARTED
  Interface.WMB_TRIP_FINISHED -> FCM.WMB_TRIP_FINISHED
  Interface.FLEET_CONSENT -> FCM.FLEET_CONSENT
  Interface.OPERATOR_CONSENT -> FCM.OPERATOR_CONSENT
  Interface.DRIVER_UNLINK_FROM_FLEET -> FCM.DRIVER_UNLINK_FROM_FLEET
  Interface.DRIVER_UNLINK_FROM_OPERATOR -> FCM.DRIVER_UNLINK_FROM_OPERATOR
  Interface.FLEET_UNLINK_FROM_OPERATOR -> FCM.FLEET_UNLINK_FROM_OPERATOR
  Interface.SAFETY_ALERT_RIDE_STOPPAGE -> FCM.SAFETY_ALERT_RIDE_STOPPAGE
  Interface.EKD_LIVE_CALL_FEEDBACK -> FCM.EKD_LIVE_CALL_FEEDBACK
  Interface.DRUNK_AND_DRIVE_VIOLATION_WARNING -> FCM.DRUNK_AND_DRIVE_VIOLATION_WARNING
  Interface.SAFETY_ALERT_RIDE_STOPPAGE_FOR_DRIVER -> FCM.SAFETY_ALERT_RIDE_STOPPAGE_FOR_DRIVER

interfaceShowNotificationToFCMShowNotification :: Interface.ShowNotification -> FCM.FCMShowNotification
interfaceShowNotificationToFCMShowNotification = \case
  Interface.SHOW -> FCM.SHOW
  Interface.DO_NOT_SHOW -> FCM.DO_NOT_SHOW
