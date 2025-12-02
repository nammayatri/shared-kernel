module Kernel.External.Notification.Interface.PayTM where

import Data.Aeson
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T hiding (map)
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
    ToJSON b,
    HasRequestId r,
    MonadReader r m
  ) =>
  PayTM.PayTMConfig ->
  Interface.NotificationReq a b ->
  m ()
notifyPerson config req = do
  case req.auth.notificationToken of
    Just notificationToken -> do
      let templateName = "PushNotification." <> show req.category <> maybe "" (("." <>) . show) req.subCategory
      let deeplinkUrl = "paytmmp://mobility?provider=nammayatri&source=notification&type=" <> show req.category <> maybe "" (("&subType=" <>) . show) req.subCategory <> T.intercalate "" (map (\(k, v) -> T.pack $ mconcat ["&", show k, "=", show v]) $ HM.toList $ toHashMap req.entity.entityData)
      let notificationData =
            PayTM.NotificationReq
              { sendBroadcastPush = True,
                notificationReceiver = PayTM.NotificationReciever "CUSTOMERID" [notificationToken],
                deviceType = [PayTM.ANDROIDAPP, PayTM.IOSAPP],
                templateName = templateName,
                dynamicParams = req.dynamicParams,
                extraCommonParams = PayTM.ExtraCommonParams "external" deeplinkUrl
              }
      PayTM.notifyPerson
        config
        notificationData
    Nothing -> logTagInfo "PayTM" $ "notificationToken of a person " <> req.auth.recipientId <> " not found"
  where
    toHashMap :: (ToJSON a) => a -> HM.HashMap Text Value
    toHashMap val = case toJSON val of
      Object obj -> AKM.toHashMapText obj
      _ -> HM.empty
