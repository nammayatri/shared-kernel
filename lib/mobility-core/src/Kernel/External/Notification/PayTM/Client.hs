{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Notification.PayTM.Client where

import EulerHS.Prelude
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification.PayTM.API as API
import qualified Kernel.External.Notification.PayTM.Types as PayTM
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common

notifyPerson ::
  ( CoreMetrics m,
    EncFlow m r,
    MonadFlow m,
    ToJSON a,
    HasRequestId r,
    MonadReader r m
  ) =>
  PayTM.PayTMConfig ->
  PayTM.NotificationReq a ->
  m ()
notifyPerson config req = do
  if (any (\el -> el == req.templateName) config.whitelistedTemplates)
    then do
      logTagDebug tag $ "request body : " <> encodeToText req -- TODO :: To Be deleted
      apiKey <- PayTM.getPaytmApiKey <$> decrypt config.apiKey
      clientId <- PayTM.getPaytmClientId <$> decrypt config.clientId
      eitherRes <- callAPI config.paytmUrl (API.notifyClient clientId apiKey req) "notifyPerson" API.notificationAPI
      case eitherRes of
        Right res ->
          case res.code of
            202 -> logTagInfo tag $ "message sent successfully to : " <> show req.notificationReceiver.notificationReceiverIdentifier
            _ -> logTagError tag $ "message failed to send to : " <> show req.notificationReceiver.notificationReceiverIdentifier
        Left _ -> logTagError tag $ "message failed to send to : " <> show req.notificationReceiver.notificationReceiverIdentifier
    else logTagInfo tag $ "message not sent as template " <> req.templateName <> " is not whitelisted"
  where
    tag = "PayTM"
