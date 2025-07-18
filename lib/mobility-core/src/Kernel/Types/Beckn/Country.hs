{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Types.Beckn.Country (Country (..)) where

import Data.Aeson
import Data.Aeson.Types
import Data.OpenApi hiding (Example)
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Storage.Esqueleto
import Kernel.Utils.GenericPretty

derivePersistField "Country"

data Country = India | France | USA | Netherlands | AnyCountry
  deriving (Eq, Generic, Show, Read, ToSchema, Ord)
  deriving (PrettyShow) via Showable Country

$(mkBeamInstancesForEnum ''Country)

instance FromJSON Country where
  parseJSON (String "IND") = pure India
  parseJSON (String "FRA") = pure France
  parseJSON (String "USA") = pure USA
  parseJSON (String "NLD") = pure Netherlands
  parseJSON (String "*") = pure AnyCountry
  parseJSON (String _) = parseFail "Invalid Country"
  parseJSON e = typeMismatch "String" e

instance ToJSON Country where
  toJSON India = String "IND"
  toJSON France = String "FRA"
  toJSON USA = String "USA"
  toJSON Netherlands = String "NLD"
  toJSON AnyCountry = String "*"
