{-# LANGUAGE DerivingStrategies #-}
{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE StandaloneDeriving #-}

module Kernel.Types.Value where

import Kernel.Prelude

newtype MandatoryValue a = MandatoryValue {value :: a}
  deriving stock (Show, Generic)

deriving anyclass instance ToJSON a => ToJSON (MandatoryValue a)

deriving anyclass instance FromJSON a => FromJSON (MandatoryValue a)

deriving anyclass instance ToSchema a => ToSchema (MandatoryValue a)

newtype OptionalValue a = OptionalValue {value :: Maybe a}
  deriving stock (Show, Generic)

deriving anyclass instance ToJSON a => ToJSON (OptionalValue a)

deriving anyclass instance FromJSON a => FromJSON (OptionalValue a)

deriving anyclass instance ToSchema a => ToSchema (OptionalValue a)
