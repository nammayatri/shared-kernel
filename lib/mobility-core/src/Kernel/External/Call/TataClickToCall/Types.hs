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

module Kernel.External.Call.TataClickToCall.Types where

import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.Text as T
import Kernel.Prelude hiding (showBaseUrl)
import Kernel.Utils.JSON
import Web.FormUrlEncoded (FromForm, ToForm)
import Web.Internal.HttpApiData

-- Click-to-Call Request
data ClickToCallRequest = ClickToCallRequest
  { agent_number :: Text,
    destination_number :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToForm, FromForm)

data ClickToCallConnectRequest = ClickToCallConnectRequest
  { agent_number :: Text,
    destination_number :: Text,
    caller_id :: Text,
    get_call_id :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToForm, FromForm)

data ClickToCallStatus
  = -- The call is ready and waiting in line before going out
    QUEUED
  | -- The call is ringing
    RINGING
  | -- The call was answered and is currently in progress
    IN_PROGRESS
  | -- The call was answered and has ended normally
    COMPLETED
  | -- The call could not be completed as dialled, most likely
    -- because the phone number was non-existent
    FAILED
  | -- The caller received a busy signal
    BUSY
  | -- The call ended without being answered
    NO_ANSWER
  | -- The call is canceled
    CANCELED
  | -- Invalid call status
    INVALID_STATUS
  | -- Knowlarity status
    CONNECTED
  | NOT_CONNECTED
  | MISSED
  | ATTEMPTED
  deriving (Show, Eq, Read, Generic, ToSchema, ToParamSchema)

instance FromHttpApiData ClickToCallStatus where
  parseUrlPiece status =
    pure case T.toLower status of
      "queued" -> QUEUED
      "ringing" -> RINGING
      "in-progress" -> IN_PROGRESS
      "completed" -> COMPLETED
      "busy" -> BUSY
      "no-answer" -> NO_ANSWER
      "failed" -> FAILED
      "canceled" -> CANCELED
      "connected" -> CONNECTED
      "not-connected" -> NOT_CONNECTED
      "missed" -> MISSED
      _ -> INVALID_STATUS

$(deriveJSON constructorsWithHyphensToLowerOptions ''ClickToCallStatus)

-- API Response (Adjust based on Tata's response format)
data ClickToCallResponse = ClickToCallResponse
  { success :: Bool,
    message :: Maybe Text,
    call_id :: Maybe Text,
    callStatus :: Maybe ClickToCallStatus
  }
  deriving (Show, Generic)

instance ToJSON ClickToCallResponse

instance FromJSON ClickToCallResponse
