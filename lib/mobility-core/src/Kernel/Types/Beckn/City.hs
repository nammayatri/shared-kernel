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
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
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
  | TamilNaduCities
  | Mysore
  | Pondicherry
  | Pune
  | Tumakuru
  | Noida
  | Gurugram
  | Tirunelveli
  | Thanjavur
  | Vellore
  | Madurai
  | Salem
  | Hosur
  | Trichy
  | AnyCity
  deriving (Eq, Generic, Show, Read, ToSchema, Ord, ToParamSchema)
  deriving (PrettyShow) via Showable City

$(mkBeamInstancesForEnumAndList ''City)

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
  parseJSON (String "std:0422") = pure TamilNaduCities
  parseJSON (String "TamilNaduCities") = pure TamilNaduCities
  parseJSON (String "std:020") = pure Pune
  parseJSON (String "Pune") = pure Pune
  parseJSON (String "std:0413") = pure Pondicherry
  parseJSON (String "Pondicherry") = pure Pondicherry
  parseJSON (String "std:0821") = pure Mysore
  parseJSON (String "Mysore") = pure Mysore
  parseJSON (String "std:0816") = pure Tumakuru
  parseJSON (String "Tumakuru") = pure Tumakuru
  parseJSON (String "std:01189") = pure Noida
  parseJSON (String "Noida") = pure Noida
  parseJSON (String "std:0124") = pure Gurugram
  parseJSON (String "Gurugram") = pure Gurugram
  parseJSON (String "std:0462") = pure Tirunelveli
  parseJSON (String "Tirunelveli") = pure Tirunelveli
  parseJSON (String "std:04362") = pure Thanjavur
  parseJSON (String "Thanjavur") = pure Thanjavur
  parseJSON (String "std:0416") = pure Vellore
  parseJSON (String "Vellore") = pure Vellore
  parseJSON (String "std:0452") = pure Madurai
  parseJSON (String "Madurai") = pure Madurai
  parseJSON (String "std:0427") = pure Salem
  parseJSON (String "Salem") = pure Salem
  parseJSON (String "std:04344") = pure Hosur
  parseJSON (String "Hosur") = pure Hosur
  parseJSON (String "std:0431") = pure Trichy
  parseJSON (String "Trichy") = pure Trichy
  parseJSON (String _) = pure AnyCity
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
  toJSON TamilNaduCities = String "std:0422"
  toJSON Pondicherry = String "std:0413"
  toJSON Pune = String "std:020"
  toJSON Mysore = String "std:0821"
  toJSON Tumakuru = String "std:0816"
  toJSON Noida = String "std:01189"
  toJSON Gurugram = String "std:0124"
  toJSON Tirunelveli = String "std:0462"
  toJSON Thanjavur = String "std:04362"
  toJSON Vellore = String "std:0416"
  toJSON Madurai = String "std:0452"
  toJSON Salem = String "std:0427"
  toJSON Hosur = String "std:04344"
  toJSON Trichy = String "std:0431"
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
      parseLowerCaseCity "std:0422" = Right TamilNaduCities
      parseLowerCaseCity "TamilNaduCities" = Right TamilNaduCities
      parseLowerCaseCity "std:020" = Right Pune
      parseLowerCaseCity "pune" = Right Pune
      parseLowerCaseCity "std:0413" = Right Pondicherry
      parseLowerCaseCity "pondicherry" = Right Pondicherry
      parseLowerCaseCity "std:0821" = Right Mysore
      parseLowerCaseCity "mysore" = Right Mysore
      parseLowerCaseCity "std:0816" = Right Tumakuru
      parseLowerCaseCity "tumakuru" = Right Tumakuru
      parseLowerCaseCity "std:01189" = Right Noida
      parseLowerCaseCity "noida" = Right Noida
      parseLowerCaseCity "std:0124" = Right Gurugram
      parseLowerCaseCity "gurugram" = Right Gurugram
      parseLowerCaseCity "std:0462" = Right Tirunelveli
      parseLowerCaseCity "tirunelveli" = Right Tirunelveli
      parseLowerCaseCity "std:04362" = Right Thanjavur
      parseLowerCaseCity "thanjavur" = Right Thanjavur
      parseLowerCaseCity "std:0416" = Right Vellore
      parseLowerCaseCity "vellore" = Right Vellore
      parseLowerCaseCity "std:0452" = Right Madurai
      parseLowerCaseCity "madurai" = Right Madurai
      parseLowerCaseCity "std:0427" = Right Salem
      parseLowerCaseCity "salem" = Right Salem
      parseLowerCaseCity "std:04344" = Right Hosur
      parseLowerCaseCity "hosur" = Right Hosur
      parseLowerCaseCity "std:0431" = Right Trichy
      parseLowerCaseCity "trichy" = Right Trichy
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
  toUrlPiece TamilNaduCities = "std:0422"
  toUrlPiece Pondicherry = "std:0413"
  toUrlPiece Pune = "std:020"
  toUrlPiece Mysore = "std:0821"
  toUrlPiece Tumakuru = "std:0816"
  toUrlPiece Noida = "std:01189"
  toUrlPiece Gurugram = "std:0124"
  toUrlPiece Tirunelveli = "std:0462"
  toUrlPiece Thanjavur = "std:04362"
  toUrlPiece Vellore = "std:0416"
  toUrlPiece Madurai = "std:0452"
  toUrlPiece Salem = "std:0427"
  toUrlPiece Hosur = "std:04344"
  toUrlPiece Trichy = "std:0431"
  toUrlPiece AnyCity = "*"
