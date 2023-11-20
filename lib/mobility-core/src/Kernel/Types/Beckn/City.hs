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

module Kernel.Types.Beckn.City (City (..)) where

import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)
import Data.OpenApi hiding (Example)
import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Storage.Esqueleto
import Kernel.Utils.GenericPretty
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))

derivePersistField "City"

data City
  = Bangalore
  | Kolkata
  | Paris
  | Kochi
  | Delhi
  | Hyderabad
  | Mumbai
  | Chennai
  | Coimbatore
  | Mysore
  | Pondicherry
  | Goa
  | Pune
  | Nashik
  | AnyCity
  deriving (Eq, Generic, Show, Read, ToSchema, Ord, ToParamSchema)
  deriving (PrettyShow) via Showable City

$(mkBeamInstancesForEnum ''City)

instance FromJSON City where
  parseJSON (String "std:080") = pure Bangalore
  parseJSON (String "Bangalore") = pure Bangalore
  parseJSON (String "std:033") = pure Kolkata
  parseJSON (String "Kolkata") = pure Kolkata
  parseJSON (String "std:001") = pure Paris
  parseJSON (String "Paris") = pure Paris
  parseJSON (String "std:484") = pure Kochi
  parseJSON (String "Kochi") = pure Kochi
  parseJSON (String "std:011") = pure Delhi
  parseJSON (String "Delhi") = pure Delhi
  parseJSON (String "std:040") = pure Hyderabad
  parseJSON (String "Hyderabad") = pure Hyderabad
  parseJSON (String "std:022") = pure Mumbai
  parseJSON (String "Mumbai") = pure Mumbai
  parseJSON (String "std:044") = pure Chennai
  parseJSON (String "Chennai") = pure Chennai
  parseJSON (String "std:0422") = pure Coimbatore
  parseJSON (String "Coimbatore") = pure Coimbatore
  parseJSON (String "std:020") = pure Pune
  parseJSON (String "Pune") = pure Pune
  parseJSON (String "std:0253") = pure Nashik
  parseJSON (String "Nashik") = pure Nashik
  parseJSON (String "std:0413") = pure Pondicherry
  parseJSON (String "Pondicherry") = pure Pondicherry
  parseJSON (String "std:08342") = pure Goa
  parseJSON (String "Goa") = pure Goa
  parseJSON (String "std:0821") = pure Mysore
  parseJSON (String "Mysore") = pure Mysore
  parseJSON (String "*") = pure AnyCity
  parseJSON (String _) = parseFail "Invalid City"
  parseJSON e = typeMismatch "String" e

instance ToJSON City where
  toJSON Bangalore = String "std:080"
  toJSON Kolkata = String "std:033"
  toJSON Paris = String "std:001"
  toJSON Kochi = String "std:484"
  toJSON Delhi = String "std:011"
  toJSON Hyderabad = String "std:040"
  toJSON Mumbai = String "std:022"
  toJSON Chennai = String "std:044"
  toJSON Coimbatore = String "std:0422"
  toJSON Pondicherry = String "std:0413"
  toJSON Goa = String "std:08342"
  toJSON Pune = String "std:020"
  toJSON Nashik = String "std:0253"
  toJSON Mysore = String "std:0821"
  toJSON AnyCity = String "*"

instance FromHttpApiData City where
  parseUrlPiece a =
    let lower = map toLower $ T.unpack a
     in parseLowerCaseCity lower
    where
      parseLowerCaseCity "std:080" = Right Bangalore
      parseLowerCaseCity "bangalore" = Right Bangalore
      parseLowerCaseCity "std:033" = Right Kolkata
      parseLowerCaseCity "kolkata" = Right Kolkata
      parseLowerCaseCity "std:001" = Right Paris
      parseLowerCaseCity "paris" = Right Paris
      parseLowerCaseCity "std:484" = Right Kochi
      parseLowerCaseCity "kochi" = Right Kochi
      parseLowerCaseCity "std:011" = Right Delhi
      parseLowerCaseCity "delhi" = Right Delhi
      parseLowerCaseCity "std:040" = Right Hyderabad
      parseLowerCaseCity "hyderabad" = Right Hyderabad
      parseLowerCaseCity "std:022" = Right Mumbai
      parseLowerCaseCity "mumbai" = Right Mumbai
      parseLowerCaseCity "std:044" = Right Chennai
      parseLowerCaseCity "chennai" = Right Chennai
      parseLowerCaseCity "std:0422" = Right Coimbatore
      parseLowerCaseCity "coimbatore" = Right Coimbatore
      parseLowerCaseCity "std:020" = Right Pune
      parseLowerCaseCity "pune" = Right Pune
      parseLowerCaseCity "std:0253" = Right Nashik
      parseLowerCaseCity "nashik" = Right Nashik
      parseLowerCaseCity "std:0413" = Right Pondicherry
      parseLowerCaseCity "pondicherry" = Right Pondicherry
      parseLowerCaseCity "std:08342" = Right Goa
      parseLowerCaseCity "goa" = Right Goa
      parseLowerCaseCity "std:0821" = Right Mysore
      parseLowerCaseCity "mysore" = Right Mysore
      parseLowerCaseCity "*" = Right AnyCity
      parseLowerCaseCity city = Left . T.pack $ ("ParseFail: Unable to parse city: " <> city)

instance ToHttpApiData City where
  toUrlPiece Bangalore = "std:080"
  toUrlPiece Kolkata = "std:033"
  toUrlPiece Paris = "std:001"
  toUrlPiece Kochi = "std:484"
  toUrlPiece Delhi = "std:011"
  toUrlPiece Hyderabad = "std:040"
  toUrlPiece Mumbai = "std:022"
  toUrlPiece Chennai = "std:044"
  toUrlPiece Coimbatore = "std:0422"
  toUrlPiece Pondicherry = "std:0413"
  toUrlPiece Goa = "std:08342"
  toUrlPiece Pune = "std:020"
  toUrlPiece Nashik = "std:0253"
  toUrlPiece Mysore = "std:0821"
  toUrlPiece AnyCity = "*"
