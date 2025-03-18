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
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Utils.JSON
import Web.FormUrlEncoded (FromForm, ToForm)
import Web.Internal.HttpApiData

-- Click-to-Call Request
data ClickToCallRequest = ClickToCallRequest
  { agent_number :: Text,
    destination_number :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToForm, FromForm)

-- API Response (Adjust based on Tata's response format)
data ClickToCallResponse = ClickToCallResponse
  { success :: Bool,
    message :: Text,
    call_id :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON ClickToCallResponse

instance FromJSON ClickToCallResponse

-- | Overall call status
data TataClickToCallStatus
  = QUEUED
  | RINGING
  | IN_PROGRESS
  | COMPLETED
  | FAILED
  | BUSY
  | NO_ANSWER
  | CANCELED
  | INVALID_STATUS
  | CONNECTED
  | NOT_CONNECTED
  | MISSED
  | ATTEMPTED
  deriving (Show, Eq, Read, Generic, ToSchema, ToParamSchema)

instance FromHttpApiData TataClickToCallStatus where
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

$(deriveJSON constructorsWithHyphensToLowerOptions ''TataClickToCallStatus)

derivePersistField "TataClickToCallStatus"
