{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Kernel.External.SMS.GupShup.Types where

import Data.Text (Text)
import Kernel.Prelude (Eq, FromJSON, Generic, HasField (hasField), Show, ToJSON)

newtype SubmitSmsRes = SubmitSmsRes
  { response :: SmsResponse
  }
  deriving (Generic, ToJSON, FromJSON, Eq, Show)

data SmsResponse = SmsResponse
  { id :: Text,
    phone :: Text,
    details :: Text,
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON, Eq, Show)
