{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Notification.GRPC.Types where

import Data.Time (UTCTime)
import EulerHS.Prelude
import Kernel.Types.Time (Seconds)
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.TH

newtype GRPCConfig = GRPCConfig
  { defaultTtl :: Seconds -- milliseconds ?
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyShow, FromJSON, ToJSON)

newtype GRPCNotificationTitle = GRPCNotificationTitle
  { getGRPCNotificationTitle :: Text
  }
  deriving (Show)
  deriving newtype (PrettyShow)

deriveIdentifierInstances ''GRPCNotificationTitle

newtype GRPCNotificationBody = GRPCNotificationBody
  { getGRPCNotificationBody :: Text
  }
  deriving (Show)
  deriving newtype (PrettyShow)

deriveIdentifierInstances ''GRPCNotificationBody

data GrpcNotificationData a = GrpcNotificationData
  { entityId :: Text,
    entityType :: Text,
    entityData :: a,
    category :: Text,
    title :: GRPCNotificationTitle,
    body :: GRPCNotificationBody,
    showNotification :: Text,
    ttl :: UTCTime,
    streamName :: Text,
    notificationId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyShow, FromJSON, ToJSON)
