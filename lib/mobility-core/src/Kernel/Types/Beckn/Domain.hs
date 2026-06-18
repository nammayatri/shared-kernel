{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Types.Beckn.Domain where

import Data.Aeson
import Data.Aeson.Types
import Data.OpenApi hiding (Example)
import Data.Singletons.TH
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.Example
import Kernel.Utils.GenericPretty

derivePersistField "Domain"

-- FIXME FRFS constructor added because in Registry instance we search in beckn_config table using `show domain`.
-- Hence should be:
-- show BecknV2.FRFS.Enums.FRFS == show Kernel.Types.Beckn.Domain.FRFS
-- toJSON BecknV2.FRFS.Enums.FRFS == toJSON Kernel.Types.Beckn.Domain.FRFS
-- When we roll out to BecknV2 completely, we can use the same domain type everywhere.
data Domain
  = MOBILITY
  | LOCAL_RETAIL
  | FOOD_AND_BEVERAGE
  | HEALTHCARE
  | METRO
  | PARKING
  | PUBLIC_TRANSPORT
  | LOGISTICS
  | FRFS
  deriving (Eq, Generic, Show, Read, FromDhall, ToSchema, Ord, ToParamSchema)
  deriving (PrettyShow) via Showable Domain

$(mkBeamInstancesForEnumAndList ''Domain)

genSingletons [''Domain]

instance Example Domain where
  example = MOBILITY

instance FromJSON Domain where
  parseJSON (String "nic2004:60221") = pure MOBILITY
  parseJSON (String "ONDC:TRV10") = pure MOBILITY
  parseJSON (String "nic2004:52110") = pure LOCAL_RETAIL
  -- parseJSON (String "nic2004:60212") = pure METRO
  parseJSON (String "nic2004:63031") = pure PARKING
  parseJSON (String "ONDC:TRV11") = pure PUBLIC_TRANSPORT
  parseJSON (String "nic2004:60232") = pure LOGISTICS
  parseJSON (String _) = parseFail "Invalid Domain"
  parseJSON e = typeMismatch "String" e

instance ToJSON Domain where
  toJSON MOBILITY = String "ONDC:TRV10"
  toJSON LOCAL_RETAIL = String "nic2004:52110"
  toJSON METRO = String "ONDC:TRV11"
  toJSON PARKING = String "nic2004:63031"
  toJSON PUBLIC_TRANSPORT = String "ONDC:TRV11"
  toJSON LOGISTICS = String "nic2004:60232"
  toJSON FRFS = String "ONDC:TRV11"
  toJSON _ = error "Invalid Domain"
