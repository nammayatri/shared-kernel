{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Types.Beckn.Country (Country (..)) where

import Data.Aeson
import Data.Aeson.Types
import Data.OpenApi hiding (Example)
import EulerHS.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Utils.GenericPretty

derivePersistField "Country"

data Country = India | France
  deriving (Eq, Generic, Show, Read, ToSchema)
  deriving (PrettyShow) via Showable Country

instance FromJSON Country where
  parseJSON (String "IN") = pure India
  parseJSON (String "FR") = pure France
  parseJSON (String _) = parseFail "Invalid Country"
  parseJSON e = typeMismatch "String" e

instance ToJSON Country where
  toJSON India = String "IN"
  toJSON France = String "FR"
