{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.OSRM.Config where

import Data.Aeson (FromJSONKey (..), ToJSONKey (..), withText)
import Data.Aeson.Types (FromJSONKeyFunction (FromJSONKeyTextParser), toJSONKeyText)
import Data.Map.Strict (Map)
import Data.OpenApi (NamedSchema (..), OpenApiType (OpenApiString), Schema (..), ToSchema (..))
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Common

data OSRMCfg = OSRMCfg
  { osrmUrl :: BaseUrl,
    radiusDeviation :: Maybe Meters,
    trafficConfig :: Maybe OSRMTrafficConfig
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data OSRMTrafficConfig = OSRMTrafficConfig
  { jamBelowKmph :: Double,
    slowBelowKmph :: Double,
    minSegmentLengthMeters :: Double,
    tierThresholds :: Maybe (Map RoadTier OSRMTrafficThresholds)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data OSRMTrafficThresholds = OSRMTrafficThresholds
  { jamBelowKmph :: Double,
    slowBelowKmph :: Double
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- These are OSRM profile classes, declared in osrm .lua files (this is done manually, not part of standard osrm)

data RoadTier = Main | Arterial | Secondary | Residential
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

roadTiers :: [RoadTier]
roadTiers = [minBound .. maxBound]

roadTierClass :: RoadTier -> Text
roadTierClass = \case
  Main -> "main"
  Arterial -> "arterial"
  Secondary -> "secondary"
  Residential -> "residential"

-- | 'Nothing' for any other class OSRM reports, e.g. @toll@ or @ferry@, which are not tiers.
roadTierFromClass :: Text -> Maybe RoadTier
roadTierFromClass className = find ((== className) . roadTierClass) roadTiers

parseRoadTier :: MonadFail m => Text -> m RoadTier
parseRoadTier className =
  maybe (fail $ "unknown road tier: " <> T.unpack className) pure $ roadTierFromClass className

instance ToJSON RoadTier where
  toJSON = toJSON . roadTierClass

instance FromJSON RoadTier where
  parseJSON = withText "RoadTier" parseRoadTier

instance ToJSONKey RoadTier where
  toJSONKey = toJSONKeyText roadTierClass

instance FromJSONKey RoadTier where
  fromJSONKey = FromJSONKeyTextParser parseRoadTier

instance ToSchema RoadTier where
  declareNamedSchema _ = pure $ NamedSchema (Just "RoadTier") tierSchema
    where
      tierSchema = mempty {_schemaType = Just OpenApiString, _schemaEnum = Just (map toJSON roadTiers)}
