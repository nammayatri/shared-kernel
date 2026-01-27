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

module Kernel.Types.Beckn.City (City (..), initCityMaps) where

import Data.Aeson
import Data.Aeson.Types
import Data.Char (isSpace)
import qualified Data.HashMap.Strict as HM
import Data.OpenApi hiding (Example, mapping)
import qualified Data.Text as T
import EulerHS.Prelude hiding (swap)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Queries.MerchantOperatingCity as Queries
import Kernel.Types.App
import Kernel.Types.CacheFlow
import Kernel.Utils.GenericPretty
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
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
      ("Bankura", "std:03242"),
      ("PurbaBardhaman", "std:0342"),
      ("AnyCity", "*")
    ]

-- Reverse mapping: std code to city name
hardcodedStdCodeToCity :: HashMap Text Text
hardcodedStdCodeToCity =
  HM.fromList $ map swap $ HM.toList hardcodedCityToStdCode
  where
    swap (k, v) = (v, k)

-- By marking a function that uses unsafePerformIO as NOINLINE,
-- you ensure that the I/O action is performed only once (at most),
-- rather than potentially multiple times if the function were inlined and
-- duplicated across various call sites.
{-# NOINLINE cityToStdCodeMap #-}
cityToStdCodeMap :: MVar (HashMap Text Text)
cityToStdCodeMap = unsafePerformIO $ newMVar HM.empty

{-# NOINLINE stdCodeToCityMap #-}
stdCodeToCityMap :: MVar (HashMap Text Text)
stdCodeToCityMap = unsafePerformIO $ newMVar HM.empty

-- Helper functions for lookups (Cache only)
getCityToStdCodeMap :: HashMap Text Text
getCityToStdCodeMap = unsafePerformIO $ do
  m <- readMVar cityToStdCodeMap
  pure $ if HM.null m then hardcodedCityToStdCode else m

getStdCodeToCityMap :: HashMap Text Text
getStdCodeToCityMap = unsafePerformIO $ do
  m <- readMVar stdCodeToCityMap
  pure $ if HM.null m then hardcodedStdCodeToCity else m

cityToStdCode :: City -> Text
cityToStdCode (City cityName) = HM.lookupDefault "*" cityName getCityToStdCodeMap

stdCodeToCity :: Text -> Maybe City
stdCodeToCity stdCode = City <$> HM.lookup stdCode getStdCodeToCityMap

initCityMaps :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => m ()
initCityMaps = do
  cache <- liftIO $ readMVar cityToStdCodeMap
  when (HM.null cache) $ do
    dbMappings <- Queries.findAll
    let dbMap = HM.fromList $ reverse $ mapMaybe (\m -> (,) (m.city) <$> m.stdCode) dbMappings
    let mergedMap = hardcodedCityToStdCode `HM.union` dbMap
    void $ liftIO $ swapMVar cityToStdCodeMap mergedMap

    let reverseDbMap = HM.fromList $ reverse $ mapMaybe (\m -> (,m.city) <$> m.stdCode) dbMappings
    let mergedReverseMap = hardcodedStdCodeToCity `HM.union` reverseDbMap
    void $ liftIO $ swapMVar stdCodeToCityMap mergedReverseMap

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
