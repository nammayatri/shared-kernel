{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Types.Beckn.City (City (..)) where

import Data.Aeson
import Data.Aeson.Types
import Data.Char (isSpace)
import qualified Data.HashMap.Strict as HM
import Data.OpenApi hiding (Example, mapping)
import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Utils.GenericPretty
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (Read (..))
import Text.Show (Show (..))

newtype City = City Text
  deriving (Eq, Generic, ToSchema, Ord, ToParamSchema)
  deriving (PrettyShow) via Showable City

instance Show City where
  show (City cityName) = T.unpack cityName

instance Read City where
  readsPrec _ s =
    let (cityName, rest) = break isSpace s
     in if null cityName
          then []
          else [(City (T.pack cityName), rest)]

derivePersistField "City"

$(mkBeamInstancesForEnumAndList ''City)

-- Hardcoded city to std code mappings
hardcodedCityToStdCode :: HashMap Text Text
hardcodedCityToStdCode =
  HM.fromList
    [ ("Bangalore", "std:080"),
      ("Kolkata", "std:033"),
      ("Paris", "std:001"),
      ("Kochi", "std:0484"),
      ("Delhi", "std:011"),
      ("Hyderabad", "std:040"),
      ("Mumbai", "std:022"),
      ("Chennai", "std:044"),
      ("TamilNaduCities", "std:0422"),
      ("Pondicherry", "std:0413"),
      ("Pune", "std:020"),
      ("Mysore", "std:0821"),
      ("Tumakuru", "std:0816"),
      ("Noida", "std:01189"),
      ("Gurugram", "std:0124"),
      ("Tirunelveli", "std:0462"),
      ("Thanjavur", "std:04362"),
      ("Vellore", "std:0416"),
      ("Madurai", "std:0452"),
      ("Salem", "std:0427"),
      ("Hosur", "std:04344"),
      ("Trichy", "std:0431"),
      ("Minneapolis", "usa:0820"),
      ("Trivandrum", "std:0471"),
      ("Thrissur", "std:0487"),
      ("Kozhikode", "std:0495"),
      ("Chandigarh", "std:0172"),
      ("Jaipur", "std:0141"),
      ("Siliguri", "std:0353"),
      ("Asansol", "std:0341"),
      ("Durgapur", "std:0342"),
      ("Petrapole", "std:03215"),
      ("Gangtok", "std:03592"),
      ("Darjeeling", "std:0354"),
      ("Davanagere", "std:08192"),
      ("Shivamogga", "std:08182"),
      ("Hubli", "std:0836"),
      ("Mangalore", "std:0824"),
      ("Udupi", "std:08200"),
      ("Gulbarga", "std:08472"),
      ("Vijayawada", "std:0866"),
      ("Vishakapatnam", "std:0891"),
      ("Guntur", "std:0863"),
      ("Tirupati", "std:0877"),
      ("Kurnool", "std:08518"),
      ("Khammam", "std:08742"),
      ("Karimnagar", "std:08722"),
      ("Nizamabad", "std:08463"),
      ("Mahbubnagar", "std:08542"),
      ("Suryapet", "std:08684"),
      ("Nalgonda", "std:08682"),
      ("Siddipet", "std:08457"),
      ("Rourkela", "std:0661"),
      ("Bhubaneshwar", "std:0674"),
      ("Cuttack", "std:0671"),
      ("Puri", "std:06752"),
      ("Warangal", "std:0870"),
      ("Pudukkottai", "std:04322"),
      ("Bidar", "std:8482"),
      ("Srinagar", "std:0194"),
      ("Alapuzha", "std:0477"),
      ("Idukki", "std:0486"),
      ("Kasaragod", "std:04994"),
      ("Wayanad", "std:04936"),
      ("Kannur", "std:0497"),
      ("Kottayam", "std:0481"),
      ("Palakkad", "std:0491"),
      ("Kollam", "std:0474"),
      ("Pathanamthitta", "std:0468"),
      ("Shillong", "std:0364"),
      ("Cherrapunji", "std:03637"),
      ("Pulwama", "std:01933"),
      ("Jammu", "std:0191"),
      ("Anantnag", "std:01932"),
      ("Berhampur", "std:0680"),
      ("Bardhaman", "std:0343"),
      ("Ballari", "std:08392"),
      ("Amsterdam", "nld:020"),
      ("Digha", "std:03216"),
      ("Jharsuguda", "std:06645"),
      ("Sambalpur", "std:0663"),
      ("Malappuram", "std:0483"),
      ("Mayiladuthurai", "std:04364"),
      ("Helsinki", "fin:009"),
      ("Turku", "fin:002"),
      ("Tampere", "fin:003"),
      ("Rajkot", "std:0281"),
      ("Somnath", "std:02871"),
      ("Dwarka", "std:02892"),
      ("Birbhum", "std:03462"),
      ("Ahmedabad", "std:079"),
      ("Surat", "std:0261"),
      ("Vadodara", "std:0265"),
      ("Jamnagar", "std:0288"),
      ("AnyCity", "*")
    ]

-- Reverse mapping: std code to city name
hardcodedStdCodeToCity :: HashMap Text Text
hardcodedStdCodeToCity =
  HM.fromList
    [ ("std:080", "Bangalore"),
      ("std:033", "Kolkata"),
      ("std:001", "Paris"),
      ("std:484", "Kochi"),
      ("std:0484", "Kochi"),
      ("std:011", "Delhi"),
      ("std:040", "Hyderabad"),
      ("std:022", "Mumbai"),
      ("std:044", "Chennai"),
      ("std:0422", "TamilNaduCities"),
      ("std:0413", "Pondicherry"),
      ("std:020", "Pune"),
      ("std:0821", "Mysore"),
      ("std:0816", "Tumakuru"),
      ("std:01189", "Noida"),
      ("std:0124", "Gurugram"),
      ("std:0462", "Tirunelveli"),
      ("std:04362", "Thanjavur"),
      ("std:0416", "Vellore"),
      ("std:0452", "Madurai"),
      ("std:0427", "Salem"),
      ("std:04344", "Hosur"),
      ("std:0431", "Trichy"),
      ("usa:0820", "Minneapolis"),
      ("std:0471", "Trivandrum"),
      ("std:0487", "Thrissur"),
      ("std:0495", "Kozhikode"),
      ("std:0172", "Chandigarh"),
      ("std:0141", "Jaipur"),
      ("std:0353", "Siliguri"),
      ("std:0341", "Asansol"),
      ("std:0342", "Durgapur"),
      ("std:03215", "Petrapole"),
      ("std:03592", "Gangtok"),
      ("std:0354", "Darjeeling"),
      ("std:08192", "Davanagere"),
      ("std:08182", "Shivamogga"),
      ("std:0836", "Hubli"),
      ("std:0824", "Mangalore"),
      ("std:08200", "Udupi"),
      ("std:08472", "Gulbarga"),
      ("std:0866", "Vijayawada"),
      ("std:0891", "Vishakapatnam"),
      ("std:0863", "Guntur"),
      ("std:0877", "Tirupati"),
      ("std:08518", "Kurnool"),
      ("std:08742", "Khammam"),
      ("std:08722", "Karimnagar"),
      ("std:08463", "Nizamabad"),
      ("std:08542", "Mahbubnagar"),
      ("std:08684", "Suryapet"),
      ("std:08682", "Nalgonda"),
      ("std:08457", "Siddipet"),
      ("std:0661", "Rourkela"),
      ("std:0674", "Bhubaneshwar"),
      ("std:0671", "Cuttack"),
      ("std:06752", "Puri"),
      ("std:0870", "Warangal"),
      ("std:04322", "Pudukkottai"),
      ("std:8482", "Bidar"),
      ("std:0194", "Srinagar"),
      ("std:0477", "Alapuzha"),
      ("std:0486", "Idukki"),
      ("std:04994", "Kasaragod"),
      ("std:04936", "Wayanad"),
      ("std:0497", "Kannur"),
      ("std:0481", "Kottayam"),
      ("std:0491", "Palakkad"),
      ("std:0474", "Kollam"),
      ("std:0468", "Pathanamthitta"),
      ("std:0364", "Shillong"),
      ("std:03637", "Cherrapunji"),
      ("std:01933", "Pulwama"),
      ("std:0191", "Jammu"),
      ("std:01932", "Anantnag"),
      ("std:0680", "Berhampur"),
      ("std:0343", "Bardhaman"),
      ("std:08392", "Ballari"),
      ("nld:020", "Amsterdam"),
      ("std:03216", "Digha"),
      ("std:06645", "Jharsuguda"),
      ("std:0663", "Sambalpur"),
      ("std:0483", "Malappuram"),
      ("std:04364", "Mayiladuthurai"),
      ("fin:009", "Helsinki"),
      ("fin:002", "Turku"),
      ("fin:003", "Tampere"),
      ("std:0281", "Rajkot"),
      ("std:02871", "Somnath"),
      ("std:02892", "Dwarka"),
      ("std:03462", "Birbhum"),
      ("std:079", "Ahmedabad"),
      ("std:0261", "Surat"),
      ("std:0265", "Vadodara"),
      ("std:0288", "Jamnagar"),
      ("*", "AnyCity")
    ]

-- Load environment variable mapping
loadEnvVarMapping :: IO (HashMap Text Text)
loadEnvVarMapping = do
  mbEnvVar <- lookupEnv "CITY_TO_STD_CODE_MAP"
  case mbEnvVar of
    Nothing -> return HM.empty
    Just envVar -> do
      case eitherDecodeStrict (encodeUtf8 $ T.pack envVar) :: Either String (HashMap Text Text) of
        Right mapping -> return mapping
        Left _ -> return HM.empty

-- By marking a function that uses unsafePerformIO as NOINLINE,
-- you ensure that the I/O action is performed only once (at most),
-- rather than potentially multiple times if the function were inlined and
-- duplicated across various call sites.
{-# NOINLINE cityToStdCodeMap #-}
cityToStdCodeMap :: MVar (HashMap Text Text)
cityToStdCodeMap = unsafePerformIO $ do
  envMapping <- loadEnvVarMapping
  let merged = HM.union envMapping hardcodedCityToStdCode
  newMVar merged

{-# NOINLINE stdCodeToCityMap #-}
stdCodeToCityMap :: MVar (HashMap Text Text)
stdCodeToCityMap = unsafePerformIO $ do
  envMapping <- loadEnvVarMapping
  -- Build reverse mapping from env var
  let envReverse = HM.fromList $ map (\(k, v) -> (v, k)) $ HM.toList envMapping
  let merged = HM.union envReverse hardcodedStdCodeToCity
  newMVar merged

-- Helper functions for lookups
getCityToStdCodeMap :: HashMap Text Text
getCityToStdCodeMap = unsafePerformIO $ readMVar cityToStdCodeMap

getStdCodeToCityMap :: HashMap Text Text
getStdCodeToCityMap = unsafePerformIO $ readMVar stdCodeToCityMap

cityToStdCode :: City -> Text
cityToStdCode (City cityName) = HM.lookupDefault "*" cityName getCityToStdCodeMap

stdCodeToCity :: Text -> Maybe City
stdCodeToCity stdCode = City <$> HM.lookup stdCode getStdCodeToCityMap

instance FromJSON City where
  parseJSON (String s) = do
    -- First try to parse as std code
    case stdCodeToCity s of
      Just city -> pure city
      Nothing -> do
        -- Then try to parse as city name (check if it exists in mapping)
        let cityName = s
        case HM.lookup cityName getCityToStdCodeMap of
          Just _ -> pure $ City cityName
          Nothing -> do
            -- Fallback: check if it's a known city name (case-insensitive)
            let lowerCityName = T.toLower cityName
            case findCityByNameIgnoreCase lowerCityName of
              Just city -> pure city
              Nothing -> pure (City "AnyCity")
    where
      findCityByNameIgnoreCase :: Text -> Maybe City
      findCityByNameIgnoreCase lowerName =
        let allCities = HM.keys getCityToStdCodeMap
            matching = filter (\c -> T.toLower c == lowerName) allCities
         in case matching of
              (c : _) -> Just $ City c
              [] -> Nothing
  parseJSON e = typeMismatch "String" e

instance ToJSON City where
  toJSON = String . cityToStdCode

instance FromHttpApiData City where
  parseUrlPiece a =
    let lowerInput = T.toLower a
        cityMap = getCityToStdCodeMap
     in case stdCodeToCity lowerInput of
          Just city -> Right city
          Nothing ->
            case findCityByNameIgnoreCase lowerInput cityMap of
              Just city -> Right city
              Nothing ->
                case HM.lookup a cityMap of
                  Just _ -> Right $ City a
                  Nothing ->
                    if lowerInput == "*" || lowerInput == "anycity"
                      then Right $ City "AnyCity"
                      else Left $ T.pack ("ParseFail: Unable to parse city: " <> T.unpack a)
    where
      findCityByNameIgnoreCase :: Text -> HashMap Text Text -> Maybe City
      findCityByNameIgnoreCase lowerName cityMap =
        let allCities = HM.keys cityMap
            matching = filter (\c -> T.toLower c == lowerName) allCities
         in case matching of
              (c : _) -> Just $ City c
              [] -> Nothing

instance ToHttpApiData City where
  toUrlPiece = cityToStdCode
