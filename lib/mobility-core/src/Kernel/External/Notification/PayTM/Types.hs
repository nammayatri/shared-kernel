{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kernel.External.Notification.PayTM.Types where

import Kernel.External.Encryption
import Kernel.Prelude

newtype PaytmApiKey = PaytmApiKey
  { getPaytmApiKey :: Text
  }
  deriving newtype (Show, Eq, ToJSON, FromJSON, DbHashable)

newtype PaytmClientId = PaytmClientId
  { getPaytmClientId :: Text
  }
  deriving newtype (Show, Eq, ToJSON, FromJSON, DbHashable)

data PayTMConfig = PayTMConfig
  { paytmUrl :: BaseUrl,
    clientId :: EncryptedField 'AsEncrypted PaytmClientId,
    apiKey :: EncryptedField 'AsEncrypted PaytmApiKey,
    whitelistedTemplates :: [Text]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data NotificationReceiverType = CUSTOMERID deriving (Generic, Show, FromJSON, ToJSON)

data NotificationReciever = NotificationReciever
  { notificationReceiverType :: NotificationReceiverType,
    notificationReceiverIdentifier :: [Text]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data DeviceType = ANDROIDAPP | IOSAPP deriving (Generic, Show, FromJSON, ToJSON)

data NotificationReq a = NotificationReq
  { sendBroadcastPush :: Bool,
    notificationReceiver :: NotificationReciever,
    deviceType :: [DeviceType],
    templateName :: Text,
    dynamicParams :: a
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Status = SUCCESS | FAILURE deriving (Generic, Show, FromJSON, ToJSON)

data NotificationResp = NotificationResp
  { code :: Int,
    message :: Text,
    status :: Status
  }
  deriving (Generic, Show, FromJSON, ToJSON)
