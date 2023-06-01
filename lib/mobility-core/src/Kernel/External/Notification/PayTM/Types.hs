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
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Notification.PayTM.Types where

import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Aeson.Types
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

data NotificationReciever = NotificationReciever
  { notificationReceiverType :: Text,
    notificationReceiverIdentifier :: [Text]
  }
  deriving (Generic, Show, ToJSON)

data DeviceType = ANDROIDAPP | IOSAPP deriving (Generic, Show, ToJSON)

data ExtraCommonParams = ExtraCommonParams
  { urlType :: Text,
    url :: Text
  }
  deriving (Generic, Show)

deriveJSON (defaultOptions {fieldLabelModifier = snakeCase}) ''ExtraCommonParams

data NotificationReq a = NotificationReq
  { sendBroadcastPush :: Bool,
    notificationReceiver :: NotificationReciever,
    deviceType :: [DeviceType],
    templateName :: Text,
    extraCommonParams :: ExtraCommonParams,
    dynamicParams :: a
  }
  deriving (Generic, Show, ToJSON)

data Status = SUCCESS | FAILURE deriving (Generic, Show, FromJSON, ToJSON)

data NotificationResp = NotificationResp
  { code :: Int,
    message :: Text,
    status :: Status
  }
  deriving (Generic, Show, FromJSON, ToJSON)
