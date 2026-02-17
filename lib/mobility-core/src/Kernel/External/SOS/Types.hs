{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveGeneric #-}

module Kernel.External.SOS.Types
  ( SOSService (..),
    availableSOSServices,
  )
where

import Data.Aeson
import Kernel.Prelude

-- | Enum of SOS service providers (extensible for future providers)
data SOSService
  = -- | C-DAC Emergency Response Support System
    ERSS
  | -- | Gujarat 112 SOS
    GJ112
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON SOSService

instance FromJSON SOSService

-- | List of all available SOS services
availableSOSServices :: [SOSService]
availableSOSServices = [ERSS, GJ112]
