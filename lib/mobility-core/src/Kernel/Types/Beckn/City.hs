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
  | Minneapolis
  | Trivandrum
  | Thrissur
  | Kozhikode
  | Chandigarh
  | Jaipur
  | Siliguri
  | Asansol
  | Durgapur
  | Petrapole
  | Gangtok
  | Darjeeling
  | Davanagere
  | Shivamogga
  | Hubli
  | Mangalore
  | Udupi
  | Gulbarga
  | Vijayawada
  | Vishakapatnam
  | Guntur
  | Tirupati
  | Kurnool
  | Khammam
  | Karimnagar
  | Nizamabad
  | Mahbubnagar
  | Suryapet
  | Nalgonda
  | Siddipet
  | Rourkela
  | Bhubaneshwar
  | Cuttack
  | Puri
  | Warangal
  | Pudukkottai
  | Bidar
  | Srinagar
  | AnyCity
  | Alapuzha
  | Idukki
  | Kasaragod
  | Wayanad
  | Kannur
  | Kottayam
  | Palakkad
  | Kollam
  | Pathanamthitta
  | Shillong
  | Cherrapunji
  | Pulwama
  | Jammu
  | Anantnag
  | Berhampur
  | Bardhaman
  | Ballari
  | Amsterdam
  | Digha
  | Jharsuguda
  | Sambalpur
  | Malappuram
  | Mayiladuthurai
  | Helsinki
  | Rajkot
  | Somnath
  | Dwarka
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
  parseJSON (String "std:0484") = pure Kochi
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
  parseJSON (String "usa:0820") = pure Minneapolis
  parseJSON (String "Minneapolis") = pure Minneapolis
  parseJSON (String "std:0471") = pure Trivandrum
  parseJSON (String "Trivandrum") = pure Trivandrum
  parseJSON (String "std:0487") = pure Thrissur
  parseJSON (String "Thrissur") = pure Thrissur
  parseJSON (String "std:0495") = pure Kozhikode
  parseJSON (String "Kozhikode") = pure Kozhikode
  parseJSON (String "std:0172") = pure Chandigarh
  parseJSON (String "Chandigarh") = pure Chandigarh
  parseJSON (String "std:0141") = pure Jaipur
  parseJSON (String "Jaipur") = pure Jaipur
  parseJSON (String "std:0353") = pure Siliguri
  parseJSON (String "Siliguri") = pure Siliguri
  parseJSON (String "std:0341") = pure Asansol
  parseJSON (String "Asansol") = pure Asansol
  parseJSON (String "std:0342") = pure Durgapur
  parseJSON (String "Durgapur") = pure Durgapur
  parseJSON (String "std:03215") = pure Petrapole
  parseJSON (String "Petrapole") = pure Petrapole
  parseJSON (String "std:03592") = pure Gangtok
  parseJSON (String "Gangtok") = pure Gangtok
  parseJSON (String "std:0354") = pure Darjeeling
  parseJSON (String "Darjeeling") = pure Darjeeling
  parseJSON (String "std:08192") = pure Davanagere
  parseJSON (String "Davanagere") = pure Davanagere
  parseJSON (String "std:08182") = pure Shivamogga
  parseJSON (String "Shivamogga") = pure Shivamogga
  parseJSON (String "std:0836") = pure Hubli
  parseJSON (String "Hubli") = pure Hubli
  parseJSON (String "std:0824") = pure Mangalore
  parseJSON (String "Mangalore") = pure Mangalore
  parseJSON (String "std:08200") = pure Udupi
  parseJSON (String "Udupi") = pure Udupi
  parseJSON (String "std:08472") = pure Gulbarga
  parseJSON (String "Gulbarga") = pure Gulbarga
  parseJSON (String "std:0866") = pure Vijayawada
  parseJSON (String "Vijayawada") = pure Vijayawada
  parseJSON (String "std:0891") = pure Vishakapatnam
  parseJSON (String "Vishakapatnam") = pure Vishakapatnam
  parseJSON (String "std:0863") = pure Guntur
  parseJSON (String "Guntur") = pure Guntur
  parseJSON (String "std:0877") = pure Tirupati
  parseJSON (String "Tirupati") = pure Tirupati
  parseJSON (String "std:08518") = pure Kurnool
  parseJSON (String "Kurnool") = pure Kurnool
  parseJSON (String "std:08742") = pure Khammam
  parseJSON (String "Khammam") = pure Khammam
  parseJSON (String "std:08722") = pure Karimnagar
  parseJSON (String "Karimnagar") = pure Karimnagar
  parseJSON (String "std:08463") = pure Nizamabad
  parseJSON (String "Nizamabad") = pure Nizamabad
  parseJSON (String "std:08542") = pure Mahbubnagar
  parseJSON (String "Mahbubnagar") = pure Mahbubnagar
  parseJSON (String "std:08684") = pure Suryapet
  parseJSON (String "Suryapet") = pure Suryapet
  parseJSON (String "std:08682") = pure Nalgonda
  parseJSON (String "Nalgonda") = pure Nalgonda
  parseJSON (String "std:08457") = pure Siddipet
  parseJSON (String "Siddipet") = pure Siddipet
  parseJSON (String "std:0661") = pure Rourkela
  parseJSON (String "Rourkela") = pure Rourkela
  parseJSON (String "std:0674") = pure Bhubaneshwar
  parseJSON (String "Bhubaneshwar") = pure Bhubaneshwar
  parseJSON (String "std:0671") = pure Cuttack
  parseJSON (String "Cuttack") = pure Cuttack
  parseJSON (String "std:06752") = pure Puri
  parseJSON (String "Puri") = pure Puri
  parseJSON (String "std:0870") = pure Warangal
  parseJSON (String "Warangal") = pure Warangal
  parseJSON (String "std:04322") = pure Pudukkottai
  parseJSON (String "Pudukkottai") = pure Pudukkottai
  parseJSON (String "std:8482") = pure Bidar
  parseJSON (String "Bidar") = pure Bidar
  parseJSON (String "std:0194") = pure Srinagar
  parseJSON (String "Srinagar") = pure Srinagar
  parseJSON (String "std:0477") = pure Alapuzha
  parseJSON (String "Alapuzha") = pure Alapuzha
  parseJSON (String "std:0486") = pure Idukki
  parseJSON (String "Idukki") = pure Idukki
  parseJSON (String "std:04994") = pure Kasaragod
  parseJSON (String "Kasaragod") = pure Kasaragod
  parseJSON (String "std:04936") = pure Wayanad
  parseJSON (String "Wayanad") = pure Wayanad
  parseJSON (String "std:0497") = pure Kannur
  parseJSON (String "Kannur") = pure Kannur
  parseJSON (String "std:0481") = pure Kottayam
  parseJSON (String "Kottayam") = pure Kottayam
  parseJSON (String "std:0491") = pure Palakkad
  parseJSON (String "Palakkad") = pure Palakkad
  parseJSON (String "std:0474") = pure Kollam
  parseJSON (String "Kollam") = pure Kollam
  parseJSON (String "std:0468") = pure Pathanamthitta
  parseJSON (String "Pathanamthitta") = pure Pathanamthitta
  parseJSON (String "std:0364") = pure Shillong
  parseJSON (String "Shillong") = pure Shillong
  parseJSON (String "std:03637") = pure Cherrapunji
  parseJSON (String "Cherrapunji") = pure Cherrapunji
  parseJSON (String "std:01933") = pure Pulwama
  parseJSON (String "Pulwama") = pure Pulwama
  parseJSON (String "std:0191") = pure Jammu
  parseJSON (String "Jammu") = pure Jammu
  parseJSON (String "std:01932") = pure Anantnag
  parseJSON (String "Anantnag") = pure Anantnag
  parseJSON (String "std:0680") = pure Berhampur
  parseJSON (String "Berhampur") = pure Berhampur
  parseJSON (String "std:0343") = pure Bardhaman
  parseJSON (String "Bardhaman") = pure Bardhaman
  parseJSON (String "std:08392") = pure Ballari
  parseJSON (String "Ballari") = pure Ballari
  parseJSON (String "nld:020") = pure Amsterdam
  parseJSON (String "Amsterdam") = pure Amsterdam
  parseJSON (String "std:03216") = pure Digha
  parseJSON (String "Digha") = pure Digha
  parseJSON (String "std:06645") = pure Jharsuguda
  parseJSON (String "Jharsuguda") = pure Jharsuguda
  parseJSON (String "std:0663") = pure Sambalpur
  parseJSON (String "Sambalpur") = pure Sambalpur
  parseJSON (String "std:0483") = pure Malappuram
  parseJSON (String "Malappuram") = pure Malappuram
  parseJSON (String "std:04364") = pure Mayiladuthurai
  parseJSON (String "Mayiladuthurai") = pure Mayiladuthurai
  parseJSON (String "fin:009") = pure Helsinki
  parseJSON (String "Helsinki") = pure Helsinki
  parseJSON (String "std:0281") = pure Rajkot
  parseJSON (String "Rajkot") = pure Rajkot
  parseJSON (String "std:02871") = pure Somnath
  parseJSON (String "Somnath") = pure Somnath
  parseJSON (String "std:02892") = pure Dwarka
  parseJSON (String "Dwarka") = pure Dwarka
  parseJSON (String _) = pure AnyCity
  parseJSON e = typeMismatch "String" e

instance ToJSON City where
  toJSON Bangalore = String "std:080"
  toJSON Kolkata = String "std:033"
  toJSON Paris = String "std:001"
  toJSON Kochi = String "std:0484"
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
  toJSON Minneapolis = String "usa:0820"
  toJSON Trivandrum = String "std:0471"
  toJSON Thrissur = String "std:0487"
  toJSON Kozhikode = String "std:0495"
  toJSON Chandigarh = String "std:0172"
  toJSON Jaipur = String "std:0141"
  toJSON Siliguri = String "std:0353"
  toJSON Asansol = String "std:0341"
  toJSON Durgapur = String "std:0342"
  toJSON Petrapole = String "std:03215"
  toJSON Gangtok = String "std:03592"
  toJSON Darjeeling = String "std:0354"
  toJSON Davanagere = String "std:08192"
  toJSON Shivamogga = String "std:08182"
  toJSON Hubli = String "std:0836"
  toJSON Mangalore = String "std:0824"
  toJSON Udupi = String "std:08200"
  toJSON Gulbarga = String "std:08472"
  toJSON Vijayawada = String "std:0866"
  toJSON Vishakapatnam = String "std:0891"
  toJSON Guntur = String "std:0863"
  toJSON Tirupati = String "std:0877"
  toJSON Kurnool = String "std:08518"
  toJSON Khammam = String "std:08742"
  toJSON Karimnagar = String "std:08722"
  toJSON Nizamabad = String "std:08463"
  toJSON Mahbubnagar = String "std:08542"
  toJSON Suryapet = String "std:08684"
  toJSON Nalgonda = String "std:08682"
  toJSON Siddipet = String "std:08457"
  toJSON Rourkela = String "std:0661"
  toJSON Bhubaneshwar = String "std:0674"
  toJSON Cuttack = String "std:0671"
  toJSON Puri = String "std:06752"
  toJSON Warangal = String "std:0870"
  toJSON Pudukkottai = String "std:04322"
  toJSON Bidar = String "std:8482"
  toJSON Srinagar = String "std:0194"
  toJSON Alapuzha = String "std:0477"
  toJSON Idukki = String "std:0486"
  toJSON Kasaragod = String "std:04994"
  toJSON Wayanad = String "std:04936"
  toJSON Kannur = String "std:0497"
  toJSON Kottayam = String "std:0481"
  toJSON Palakkad = String "std:0491"
  toJSON Kollam = String "std:0474"
  toJSON Pathanamthitta = String "std:0468"
  toJSON Shillong = String "std:0364"
  toJSON Cherrapunji = String "std:03637"
  toJSON Pulwama = String "std:01933"
  toJSON Jammu = String "std:0191"
  toJSON Anantnag = String "std:01932"
  toJSON Berhampur = String "std:0680"
  toJSON Bardhaman = String "std:0343"
  toJSON Ballari = String "std:08392"
  toJSON Amsterdam = String "nld:020"
  toJSON Digha = String "std:03216"
  toJSON Jharsuguda = String "std:06645"
  toJSON Sambalpur = String "std:0663"
  toJSON Malappuram = String "std:0483"
  toJSON Mayiladuthurai = String "std:04364"
  toJSON Helsinki = String "fin:009"
  toJSON Rajkot = String "std:0281"
  toJSON Somnath = String "std:02871"
  toJSON Dwarka = String "std:02892"
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
      parseLowerCaseCity "std:0484" = Right Kochi
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
      parseLowerCaseCity "tamilnaducities" = Right TamilNaduCities
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
      parseLowerCaseCity "usa:0820" = Right Minneapolis
      parseLowerCaseCity "minneapolis" = Right Minneapolis
      parseLowerCaseCity "std:0471" = Right Trivandrum
      parseLowerCaseCity "trivandrum" = Right Trivandrum
      parseLowerCaseCity "std:0487" = Right Thrissur
      parseLowerCaseCity "thrissur" = Right Thrissur
      parseLowerCaseCity "std:0495" = Right Kozhikode
      parseLowerCaseCity "kozhikode" = Right Kozhikode
      parseLowerCaseCity "std:0172" = Right Chandigarh
      parseLowerCaseCity "chandigarh" = Right Chandigarh
      parseLowerCaseCity "std:0141" = Right Jaipur
      parseLowerCaseCity "jaipur" = Right Jaipur
      parseLowerCaseCity "std:0353" = Right Siliguri
      parseLowerCaseCity "siliguri" = Right Siliguri
      parseLowerCaseCity "std:0341" = Right Asansol
      parseLowerCaseCity "asansol" = Right Asansol
      parseLowerCaseCity "std:0342" = Right Durgapur
      parseLowerCaseCity "durgapur" = Right Durgapur
      parseLowerCaseCity "std:03215" = Right Petrapole
      parseLowerCaseCity "petrapole" = Right Petrapole
      parseLowerCaseCity "std:03592" = Right Gangtok
      parseLowerCaseCity "gangtok" = Right Gangtok
      parseLowerCaseCity "std:0354" = Right Darjeeling
      parseLowerCaseCity "darjeeling" = Right Darjeeling
      parseLowerCaseCity "std:08192" = Right Davanagere
      parseLowerCaseCity "davanagere" = Right Davanagere
      parseLowerCaseCity "std:08182" = Right Shivamogga
      parseLowerCaseCity "shivamogga" = Right Shivamogga
      parseLowerCaseCity "std:0836" = Right Hubli
      parseLowerCaseCity "hubli" = Right Hubli
      parseLowerCaseCity "std:0824" = Right Mangalore
      parseLowerCaseCity "mangalore" = Right Mangalore
      parseLowerCaseCity "std:08200" = Right Udupi
      parseLowerCaseCity "udupi" = Right Udupi
      parseLowerCaseCity "std:08472" = Right Gulbarga
      parseLowerCaseCity "gulbarga" = Right Gulbarga
      parseLowerCaseCity "std:0866" = Right Vijayawada
      parseLowerCaseCity "vijayawada" = Right Vijayawada
      parseLowerCaseCity "std:0891" = Right Vishakapatnam
      parseLowerCaseCity "vishakapatnam" = Right Vishakapatnam
      parseLowerCaseCity "std:0863" = Right Guntur
      parseLowerCaseCity "guntur" = Right Guntur
      parseLowerCaseCity "std:0877" = Right Tirupati
      parseLowerCaseCity "tirupati" = Right Tirupati
      parseLowerCaseCity "std:08518" = Right Kurnool
      parseLowerCaseCity "kurnool" = Right Kurnool
      parseLowerCaseCity "std:08742" = Right Khammam
      parseLowerCaseCity "khammam" = Right Khammam
      parseLowerCaseCity "std:08722" = Right Karimnagar
      parseLowerCaseCity "karimnagar" = Right Karimnagar
      parseLowerCaseCity "std:08463" = Right Nizamabad
      parseLowerCaseCity "nizamabad" = Right Nizamabad
      parseLowerCaseCity "std:08542" = Right Mahbubnagar
      parseLowerCaseCity "mahbubnagar" = Right Mahbubnagar
      parseLowerCaseCity "std:08684" = Right Suryapet
      parseLowerCaseCity "suryapet" = Right Suryapet
      parseLowerCaseCity "std:08682" = Right Nalgonda
      parseLowerCaseCity "nalgonda" = Right Nalgonda
      parseLowerCaseCity "std:08457" = Right Siddipet
      parseLowerCaseCity "siddipet" = Right Siddipet
      parseLowerCaseCity "std:0661" = Right Rourkela
      parseLowerCaseCity "rourkela" = Right Rourkela
      parseLowerCaseCity "std:0674" = Right Bhubaneshwar
      parseLowerCaseCity "bhubaneshwar" = Right Bhubaneshwar
      parseLowerCaseCity "std:0671" = Right Cuttack
      parseLowerCaseCity "cuttack" = Right Cuttack
      parseLowerCaseCity "std:06752" = Right Puri
      parseLowerCaseCity "puri" = Right Puri
      parseLowerCaseCity "std:0870" = Right Warangal
      parseLowerCaseCity "warangal" = Right Warangal
      parseLowerCaseCity "std:04322" = Right Pudukkottai
      parseLowerCaseCity "pudukkottai" = Right Pudukkottai
      parseLowerCaseCity "std:8482" = Right Bidar
      parseLowerCaseCity "bidar" = Right Bidar
      parseLowerCaseCity "std:0194" = Right Srinagar
      parseLowerCaseCity "srinagar" = Right Srinagar
      parseLowerCaseCity "std:0477" = Right Alapuzha
      parseLowerCaseCity "alapuzha" = Right Alapuzha
      parseLowerCaseCity "std:0486" = Right Idukki
      parseLowerCaseCity "idukki" = Right Idukki
      parseLowerCaseCity "std:04994" = Right Kasaragod
      parseLowerCaseCity "kasaragod" = Right Kasaragod
      parseLowerCaseCity "std:04936" = Right Wayanad
      parseLowerCaseCity "wayanad" = Right Wayanad
      parseLowerCaseCity "std:0497" = Right Kannur
      parseLowerCaseCity "kannur" = Right Kannur
      parseLowerCaseCity "std:0481" = Right Kottayam
      parseLowerCaseCity "kottayam" = Right Kottayam
      parseLowerCaseCity "std:0491" = Right Palakkad
      parseLowerCaseCity "palakkad" = Right Palakkad
      parseLowerCaseCity "std:0474" = Right Kollam
      parseLowerCaseCity "kollam" = Right Kollam
      parseLowerCaseCity "std:0468" = Right Pathanamthitta
      parseLowerCaseCity "pathanamthitta" = Right Pathanamthitta
      parseLowerCaseCity "std:0364" = Right Shillong
      parseLowerCaseCity "shillong" = Right Shillong
      parseLowerCaseCity "std:03637" = Right Cherrapunji
      parseLowerCaseCity "cherrapunji" = Right Cherrapunji
      parseLowerCaseCity "std:01933" = Right Pulwama
      parseLowerCaseCity "pulwama" = Right Pulwama
      parseLowerCaseCity "std:0191" = Right Jammu
      parseLowerCaseCity "jammu" = Right Jammu
      parseLowerCaseCity "std:01932" = Right Anantnag
      parseLowerCaseCity "anantnag" = Right Anantnag
      parseLowerCaseCity "std:0680" = Right Berhampur
      parseLowerCaseCity "berhampur" = Right Berhampur
      parseLowerCaseCity "std:0343" = Right Bardhaman
      parseLowerCaseCity "bardhaman" = Right Bardhaman
      parseLowerCaseCity "std:08392" = Right Ballari
      parseLowerCaseCity "ballari" = Right Ballari
      parseLowerCaseCity "nld:020" = Right Amsterdam
      parseLowerCaseCity "amsterdam" = Right Amsterdam
      parseLowerCaseCity "std:03216" = Right Digha
      parseLowerCaseCity "digha" = Right Digha
      parseLowerCaseCity "std:06645" = Right Jharsuguda
      parseLowerCaseCity "jharsuguda" = Right Jharsuguda
      parseLowerCaseCity "std:0663" = Right Sambalpur
      parseLowerCaseCity "sambalpur" = Right Sambalpur
      parseLowerCaseCity "std:0483" = Right Malappuram
      parseLowerCaseCity "malappuram" = Right Malappuram
      parseLowerCaseCity "std:04364" = Right Mayiladuthurai
      parseLowerCaseCity "mayiladuthurai" = Right Mayiladuthurai
      parseLowerCaseCity "fin:009" = Right Helsinki
      parseLowerCaseCity "helsinki" = Right Helsinki
      parseLowerCaseCity "std:0281" = Right Rajkot
      parseLowerCaseCity "rajkot" = Right Rajkot
      parseLowerCaseCity "std:02871" = Right Somnath
      parseLowerCaseCity "somnath" = Right Somnath
      parseLowerCaseCity "std:02892" = Right Dwarka
      parseLowerCaseCity "dwarka" = Right Dwarka
      parseLowerCaseCity "*" = Right AnyCity
      parseLowerCaseCity city = Left . T.pack $ ("ParseFail: Unable to parse city: " <> city)

instance ToHttpApiData City where
  toUrlPiece Bangalore = "std:080"
  toUrlPiece Kolkata = "std:033"
  toUrlPiece Paris = "std:001"
  toUrlPiece Kochi = "std:0484"
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
  toUrlPiece Minneapolis = "usa:0820"
  toUrlPiece Trivandrum = "std:0471"
  toUrlPiece Thrissur = "std:0487"
  toUrlPiece Kozhikode = "std:0495"
  toUrlPiece Chandigarh = "std:0172"
  toUrlPiece Jaipur = "std:0141"
  toUrlPiece Siliguri = "std:0353"
  toUrlPiece Asansol = "std:0341"
  toUrlPiece Durgapur = "std:0342"
  toUrlPiece Petrapole = "std:03215"
  toUrlPiece Gangtok = "std:03592"
  toUrlPiece Darjeeling = "std:0354"
  toUrlPiece Davanagere = "std:08192"
  toUrlPiece Shivamogga = "std:08182"
  toUrlPiece Hubli = "std:0836"
  toUrlPiece Mangalore = "std:0824"
  toUrlPiece Udupi = "std:08200"
  toUrlPiece Gulbarga = "std:08472"
  toUrlPiece Vijayawada = "std:0866"
  toUrlPiece Vishakapatnam = "std:0891"
  toUrlPiece Guntur = "std:0863"
  toUrlPiece Tirupati = "std:0877"
  toUrlPiece Kurnool = "std:08518"
  toUrlPiece Khammam = "std:08742"
  toUrlPiece Karimnagar = "std:08722"
  toUrlPiece Nizamabad = "std:08463"
  toUrlPiece Mahbubnagar = "std:08542"
  toUrlPiece Suryapet = "std:08684"
  toUrlPiece Nalgonda = "std:08682"
  toUrlPiece Siddipet = "std:08457"
  toUrlPiece Rourkela = "std:0661"
  toUrlPiece Bhubaneshwar = "std:0674"
  toUrlPiece Cuttack = "std:0671"
  toUrlPiece Puri = "std:06752"
  toUrlPiece Warangal = "std:0870"
  toUrlPiece Pudukkottai = "std:04322"
  toUrlPiece Bidar = "std:8482"
  toUrlPiece Srinagar = "std:0194"
  toUrlPiece Alapuzha = "std:0477"
  toUrlPiece Idukki = "std:0486"
  toUrlPiece Kasaragod = "std:04994"
  toUrlPiece Wayanad = "std:04936"
  toUrlPiece Kannur = "std:0497"
  toUrlPiece Kottayam = "std:0481"
  toUrlPiece Palakkad = "std:0491"
  toUrlPiece Kollam = "std:0474"
  toUrlPiece Pathanamthitta = "std:0468"
  toUrlPiece Shillong = "std:0364"
  toUrlPiece Cherrapunji = "std:03637"
  toUrlPiece Pulwama = "std:01933"
  toUrlPiece Jammu = "std:0191"
  toUrlPiece Anantnag = "std:01932"
  toUrlPiece Berhampur = "std:0680"
  toUrlPiece Bardhaman = "std:0343"
  toUrlPiece Ballari = "std:08392"
  toUrlPiece Amsterdam = "nld:020"
  toUrlPiece Helsinki = "fin:009"
  toUrlPiece Digha = "std:03216"
  toUrlPiece Jharsuguda = "std:06645"
  toUrlPiece Sambalpur = "std:0663"
  toUrlPiece Malappuram = "std:0483"
  toUrlPiece Mayiladuthurai = "std:04364"
  toUrlPiece Rajkot = "std:0281"
  toUrlPiece Somnath = "std:02871"
  toUrlPiece Dwarka = "std:02892"
  toUrlPiece AnyCity = "*"
