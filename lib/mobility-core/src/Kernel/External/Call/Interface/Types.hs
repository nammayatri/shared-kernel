{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Call.Interface.Types where

import qualified Kernel.External.Call.Exotel.Config as Exotel
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)

newtype CallServiceConfig = ExotelConfig Exotel.ExotelCfg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data InitiateCallReq a = InitiateCallReq
  { fromPhoneNum :: Text,
    toPhoneNum :: Text,
    attachments :: Attachments a
  }
  deriving (Generic)

newtype Attachments a = Attachments
  { getAttachments :: a
  }
  deriving (Generic)

data CallStatus
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
  deriving (Generic, Show, Read, ToJSON, FromJSON, ToSchema)

derivePersistField "CallStatus"

data InitiateCallResp = InitiateCallResp
  { callId :: Text,
    callStatus :: CallStatus
  }
  deriving (Generic)
