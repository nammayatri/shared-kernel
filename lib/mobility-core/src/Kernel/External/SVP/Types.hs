{-
  Copyright 2022-25, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveGeneric #-}

module Kernel.External.SVP.Types
  ( SVPService (..),
    SVPConfig (..),
  )
where

import Data.Aeson
import Kernel.Prelude

data SVPService = Juspay
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data SVPConfig = SVPConfig
  { programId :: Text,
    burnOptionId :: Text,
    gatewayId :: Text,
    paymentMethod :: Text,
    paymentMethodType :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
