{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Kernel.External.SMS.TwillioSms.Types where

import Data.Aeson
import Data.Aeson.TH
import Kernel.Prelude
import Kernel.Utils.JSON
import Web.FormUrlEncoded (ToForm, toForm)
import Web.Internal.HttpApiData

data TwillioSmsStatus = ACCEPTED | SCHEDULED | CANCELED | QUEUED | SENDING | SENT | FAILED | DELIVERED | UNDELIVERED | RECEIVING | RECEIVED | READ
  deriving (Generic, Show, Eq, ToSchema)

$(deriveJSON constructorsToLowerOptions ''TwillioSmsStatus)

data TwillioSmsDirection = INBOUND | OUTBOUND_API | OUTBOUND_CALL | OUTBOUND_REPLY
  deriving (Generic, Show, Eq, ToSchema)

$(deriveJSON constructorsWithHyphensToLowerOptions ''TwillioSmsDirection)

data TwillioSmsResp = TwillioSmsResp
  { apiVersion :: Text,
    body :: Text,
    dateCreated :: Text,
    direction :: TwillioSmsDirection,
    errorCode :: Maybe Text,
    errorMessage :: Maybe Text,
    from :: Maybe Text,
    numSegments :: Text,
    sid :: Text,
    status :: TwillioSmsStatus,
    to :: Text
  }
  deriving (Generic, Show, Eq, ToSchema)

instance ToJSON TwillioSmsResp where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON TwillioSmsResp where
  parseJSON = genericParseJSON constructorsWithSnakeCase

data TwillioSmsReq = TwillioSmsReq
  { to :: Text,
    body :: Text,
    messagingServiceSid :: Text,
    from :: Text
  }
  deriving (Generic, ToJSON, FromJSON, Eq, ToSchema)

instance ToForm TwillioSmsReq where
  toForm TwillioSmsReq {..} =
    [ ("To", toQueryParam to),
      ("Body", toQueryParam body),
      ("MessagingServiceSid", toQueryParam messagingServiceSid),
      ("From", toQueryParam from)
    ]
