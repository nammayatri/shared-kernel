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

module Kernel.Types.Beckn.State (State (..)) where

import Data.Aeson
import Data.Aeson.Types
import Data.OpenApi hiding (Example)
import EulerHS.Prelude hiding (State)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto
import Kernel.Utils.GenericPretty

derivePersistField "State"

data State
  = AndhraPradesh
  | ArunachalPradesh
  | Assam
  | Bihar
  | Chhattisgarh
  | Goa
  | Gujarat
  | Haryana
  | HimachalPradesh
  | Jharkhand
  | Karnataka
  | Kerala
  | MadhyaPradesh
  | Maharashtra
  | Manipur
  | Meghalaya
  | Mizoram
  | Nagaland
  | Odisha
  | Punjab
  | Rajasthan
  | Sikkim
  | TamilNadu
  | Telangana
  | Tripura
  | UttarPradesh
  | Uttarakhand
  | WestBengal
  | AndamanAndNicobarIslands
  | Chandigarh
  | DadraAndNagarHaveli
  | DamanAndDiu
  | DelhiTerritory
  | Lakshadweep
  | Puducherry
  | AnyState
  deriving (Eq, Generic, Show, Read, ToSchema, Ord)
  deriving (PrettyShow) via Showable State

$(mkBeamInstancesForEnumAndList ''State)

instance FromJSON State where
  parseJSON (String "AndhraPradesh") = pure AndhraPradesh
  parseJSON (String "ArunachalPradesh") = pure ArunachalPradesh
  parseJSON (String "Assam") = pure Assam
  parseJSON (String "Bihar") = pure Bihar
  parseJSON (String "Chhattisgarh") = pure Chhattisgarh
  parseJSON (String "Goa") = pure Goa
  parseJSON (String "Gujarat") = pure Gujarat
  parseJSON (String "Haryana") = pure Haryana
  parseJSON (String "HimachalPradesh") = pure HimachalPradesh
  parseJSON (String "Jharkhand") = pure Jharkhand
  parseJSON (String "Karnataka") = pure Karnataka
  parseJSON (String "Kerala") = pure Kerala
  parseJSON (String "MadhyaPradesh") = pure MadhyaPradesh
  parseJSON (String "Maharashtra") = pure Maharashtra
  parseJSON (String "Manipur") = pure Manipur
  parseJSON (String "Meghalaya") = pure Meghalaya
  parseJSON (String "Mizoram") = pure Mizoram
  parseJSON (String "Nagaland") = pure Nagaland
  parseJSON (String "Odisha") = pure Odisha
  parseJSON (String "Punjab") = pure Punjab
  parseJSON (String "Rajasthan") = pure Rajasthan
  parseJSON (String "Sikkim") = pure Sikkim
  parseJSON (String "TamilNadu") = pure TamilNadu
  parseJSON (String "Telangana") = pure Telangana
  parseJSON (String "Tripura") = pure Tripura
  parseJSON (String "UttarPradesh") = pure UttarPradesh
  parseJSON (String "Uttarakhand") = pure Uttarakhand
  parseJSON (String "WestBengal") = pure WestBengal
  parseJSON (String "AndamanAndNicobarIslands") = pure AndamanAndNicobarIslands
  parseJSON (String "Chandigarh") = pure Chandigarh
  parseJSON (String "DadraAndNagarHaveli") = pure DadraAndNagarHaveli
  parseJSON (String "DamanAndDiu") = pure DamanAndDiu
  parseJSON (String "DelhiTerritory") = pure DelhiTerritory
  parseJSON (String "Lakshadweep") = pure Lakshadweep
  parseJSON (String "Puducherry") = pure Puducherry
  parseJSON (String "*") = pure AnyState
  parseJSON (String _) = parseFail "Invalid State"
  parseJSON e = typeMismatch "String" e

instance ToJSON State where
  toJSON ArunachalPradesh = String "ArunachalPradesh"
  toJSON AndhraPradesh = String "AndhraPradesh"
  toJSON Assam = String "Assam"
  toJSON Bihar = String "Bihar"
  toJSON Chhattisgarh = String "Chhattisgarh"
  toJSON Goa = String "Goa"
  toJSON Gujarat = String "Gujarat"
  toJSON Haryana = String "Haryana"
  toJSON HimachalPradesh = String "HimachalPradesh"
  toJSON Jharkhand = String "Jharkhand"
  toJSON Karnataka = String "Karnataka"
  toJSON Kerala = String "Kerala"
  toJSON MadhyaPradesh = String "MadhyaPradesh"
  toJSON Maharashtra = String "Maharashtra"
  toJSON Manipur = String "Manipur"
  toJSON Meghalaya = String "Meghalaya"
  toJSON Mizoram = String "Mizoram"
  toJSON Nagaland = String "Nagaland"
  toJSON Odisha = String "Odisha"
  toJSON Punjab = String "Punjab"
  toJSON Rajasthan = String "Rajasthan"
  toJSON Sikkim = String "Sikkim"
  toJSON TamilNadu = String "TamilNadu"
  toJSON Telangana = String "Telangana"
  toJSON Tripura = String "Tripura"
  toJSON UttarPradesh = String "UttarPradesh"
  toJSON Uttarakhand = String "Uttarakhand"
  toJSON WestBengal = String "WestBengal"
  toJSON AndamanAndNicobarIslands = String "AndamanAndNicobarIslands"
  toJSON Chandigarh = String "Chandigarh"
  toJSON DadraAndNagarHaveli = String "DadraAndNagarHaveli"
  toJSON DamanAndDiu = String "DamanAndDiu"
  toJSON DelhiTerritory = String "DelhiTerritory"
  toJSON Lakshadweep = String "Lakshadweep"
  toJSON Puducherry = String "Puducherry"
  toJSON AnyState = String "*"
