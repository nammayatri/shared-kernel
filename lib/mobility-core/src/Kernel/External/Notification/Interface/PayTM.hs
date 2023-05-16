module Kernel.External.Notification.Interface.PayTM where

import qualified Kernel.External.Notification.Interface.Types as Interface
import qualified Kernel.External.Notification.PayTM.Client as PayTM
import qualified Kernel.External.Notification.PayTM.Types as PayTM
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common

notifyPerson ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    ToJSON a,
    ToJSON b
  ) =>
  PayTM.PayTMConfig ->
  Interface.NotificationReq a b ->
  m ()
notifyPerson config req = do
  case req.auth.notificationToken of
    Just notificationToken -> do
      let templateName = "PushNotification." <> show req.category <> maybe "" (("." <>) . show) req.subCategory
      let notificationData =
            PayTM.NotificationReq
              { sendBroadcastPush = True,
                notificationReceiver = PayTM.NotificationReciever PayTM.CUSTOMERID [notificationToken],
                deviceType = [PayTM.ANDROIDAPP, PayTM.IOSAPP],
                templateName = templateName,
                dynamicParams = req.dynamicParams
              }
      PayTM.notifyPerson
        config
        notificationData
    Nothing -> logTagInfo "PayTM" $ "notificationToken of a person " <> req.auth.recipientId <> " not found"
