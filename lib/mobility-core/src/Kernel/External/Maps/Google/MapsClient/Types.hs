{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Kernel.External.Maps.Google.MapsClient.Types where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Double.Conversion.Text (toFixed)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Kernel.External.Maps.Google.PolyLinePoints (PolyLinePoints)
import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)
import Servant (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))

data AutoCompleteResp = AutoCompleteResp
  { status :: Text,
    predictions :: [Prediction]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Prediction = Prediction
  { description :: Text,
    place_id :: Maybe Text,
    distance_meters :: Maybe Int,
    types :: Maybe [Text]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype AutoCompleteRespV2 = AutoCompleteRespV2
  { suggestions :: [Suggestion]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype Suggestion = Suggestion
  { placePrediction :: PlacePrediction
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PlacePrediction = PlacePrediction
  { text :: PlaceText,
    placeId :: Maybe Text,
    types :: Maybe [Text],
    distanceMeters :: Maybe Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype PlaceText = PlaceText
  { text :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data GetPlaceDetailsResp = GetPlaceDetailsResp
  { status :: Text,
    result :: PlaceDetailsResult
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype PlaceDetailsResult = PlaceDetailsResult
  { geometry :: Geometry
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype Geometry = Geometry
  { location :: LocationS
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationS = LocationS
  { lat :: Double,
    lng :: Double
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

instance ToHttpApiData LocationS where
  toUrlPiece (LocationS lat' lng') =
    T.concat [toFixed precision lat', ",", toFixed precision lng']
    where
      precision = 6 -- Precision beyond 6 decimal places is ignored.

instance FromHttpApiData LocationS where
  parseUrlPiece param = do
    let latLon = T.splitOn "," param
    case latLon of
      [latText, lonText] -> do
        lat <- left T.pack . eitherDecode . BSL.fromStrict . DT.encodeUtf8 $ latText
        lng <- left T.pack . eitherDecode . BSL.fromStrict . DT.encodeUtf8 $ lonText
        Right LocationS {..}
      _ -> Left "location should contain lat and lon separated by a comma"

data GetPlaceNameResp = GetPlaceNameResp
  { status :: Text,
    results :: [ResultsResp]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ResultsResp = ResultsResp
  { formatted_address :: Maybe Text,
    address_components :: [AddressResp],
    plus_code :: Maybe PlusCodeResp,
    geometry :: Geometry,
    place_id :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AddressResp = AddressResp
  { long_name :: Text,
    short_name :: Text,
    types :: [Text]
  }
  deriving stock (Generic, Show, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PlusCodeResp = PlusCodeResp
  { compound_code :: Maybe Text,
    global_code :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DirectionsResp = DirectionsResp
  { routes :: [Route],
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype AdvancedDirectionsResp = AdvancedDirectionsResp
  { routes :: [RouteV2]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data AutoCompleteReqV2 = AutoCompleteReqV2
  { input :: Text,
    sessionToken :: Maybe Text,
    origin :: Maybe LatLngV2,
    includedPrimaryTypes :: Maybe Text,
    includedRegionCodes :: [Text],
    locationBias :: Maybe LocationBias,
    locationRestriction :: Maybe LocationRestriction
  }
  deriving (Generic, FromJSON, ToJSON)

newtype LocationBias = LocationBias
  { circle :: Circle --,
  -- rectangle :: Maybe Rectangle  -----------can be used in future-----------
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype LocationRestriction = LocationRestriction
  { circle :: Circle --,
  -- rectangle :: Maybe Rectangle  -----------can be used in future-----------
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Circle = Circle
  { center :: LatLngV2,
    radius :: Double
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Rectangle = Rectangle
  { low :: LatLngV2,
    high :: LatLngV2
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data AdvancedDirectionsReq = AdvancedDirectionsReq
  { origin :: WayPointV2,
    destination :: WayPointV2,
    intermediates :: Maybe [WayPointV2],
    routingPreference :: RoutingPreference,
    travelMode :: Maybe ModeV2,
    computeAlternativeRoutes :: Bool,
    routeModifiers :: RouteModifiers
  }
  -----------remove null fields-----------
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype WayPointV2 = WayPointV2
  { ---- Union field location_type can be only one of the following:
    location :: LocationV2
    -- address :: Maybe Text --------if needed in future--------
    -----End of list of possible types for union field location_type.
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype LocationV2 = LocationV2
  { latLng :: LatLngV2
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data LatLngV2 = LatLngV2
  { latitude :: Double,
    longitude :: Double
  }
  deriving (Generic, Eq, ToJSON, FromJSON, ToSchema, Show)

data RouteModifiers = RouteModifiers
  { avoidTolls :: Maybe Bool,
    avoidFerries :: Bool --------default False--------
    ------- Can be added in future if required ----------
    -- avoidHighways :: Bool,
    -- avoidIndoor :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Route = Route
  { bounds :: Bounds,
    legs :: [Leg]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Bounds = Bounds
  { northeast :: LocationS,
    southwest :: LocationS
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Leg = Leg
  { distance :: TextValue,
    duration :: TextValue,
    end_location :: LocationS,
    start_location :: LocationS,
    steps :: [Step]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Step = Step
  { distance :: TextValue,
    duration :: TextValue,
    end_location :: LocationS,
    polyline :: EncodedPointObject,
    start_location :: LocationS,
    travel_mode :: Mode
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data RouteV2 = RouteV2
  { legs :: [LegV2],
    viewport :: ViewPort,
    distanceMeters :: Int,
    duration :: Text,
    staticDuration :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ViewPort = ViewPort
  { low :: LatLngV2,
    high :: LatLngV2
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data LegV2 = LegV2
  { distanceMeters :: Int,
    duration :: Text,
    endLocation :: LocationV2,
    startLocation :: LocationV2,
    steps :: [StepV2]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data StepV2 = StepV2
  { distanceMeters :: Int,
    staticDuration :: Text,
    endLocation :: LocationV2,
    polyline :: Polyline,
    startLocation :: LocationV2,
    travelMode :: ModeV2
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype Polyline = Polyline
  { encodedPolyline :: PolyLinePoints
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype EncodedPointObject = EncodedPointObject {points :: PolyLinePoints}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DistanceMatrixResp = DistanceMatrixResp
  { destination_addresses :: [Text],
    origin_addresses :: [Text],
    rows :: [DistanceMatrixRow],
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

newtype DistanceMatrixRow = DistanceMatrixRow {elements :: [DistanceMatrixElement]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data DistanceMatrixElement = DistanceMatrixElement
  { distance :: Maybe TextValue,
    duration :: Maybe TextValue,
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

data TextValue = TextValue
  { text :: Text,
    value :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Place = Location LocationS | Address Text deriving (Show)

instance ToHttpApiData Place where
  toUrlPiece (Location location) = toUrlPiece location
  toUrlPiece (Address address) = address

instance FromHttpApiData Place where
  parseUrlPiece piece = case parseUrlPiece piece of
    Right location -> Right $ Location location
    _ -> Right $ Address piece

instance ToHttpApiData [Place] where
  toUrlPiece latLongList = T.intercalate "|" $ toUrlPiece <$> latLongList

instance FromHttpApiData [Place] where
  parseUrlPiece piece = do
    let places = T.splitOn "|" piece
    forM places parseUrlPiece

data Mode = DRIVING | WALKING | BICYCLING
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data ModeV2
  = TRAVEL_MODE_UNSPECIFIED -- No travel mode specified. Defaults to DRIVE.
  | DRIVE --Travel by passenger car.
  | BICYCLE -- Travel by bicycle.
  | WALK -- Travel by walking.
  | TWO_WHEELER -- Two-wheeled, motorized vehicle. For example, motorcycle. Note that this differs from the BICYCLE travel mode which covers human-powered mode.
  | TRANSIT -- Travel by public transit.
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data RoutingPreference = ROUTING_PREFERENCE_UNSPECIFIED | TRAFFIC_UNAWARE | TRAFFIC_AWARE | TRAFFIC_AWARE_OPTIMAL
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema, FromDhall)

instance ToHttpApiData Mode where
  toUrlPiece = T.toLower . show

instance FromHttpApiData Mode where
  parseUrlPiece = left T.pack . eitherDecode .(\str -> "\"" <> str <> "\"") . BSL.fromStrict . DT.encodeUtf8 . T.toUpper

data DepartureTime = Now | FutureTime UTCTime

instance ToHttpApiData DepartureTime where
  toUrlPiece Now = "now"
  toUrlPiece (FutureTime time) = show time
