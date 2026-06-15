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
import qualified Kernel.Utils.JSON as JS
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
  deriving (Show, Generic, ToJSON, ToSchema)

instance FromJSON AutoCompleteRespV2 where
  parseJSON (Object v) = do
    suggestionsValue <- v .:? "suggestions" .!= []
    return $ AutoCompleteRespV2 suggestionsValue
  parseJSON _ = fail "Expected an object for AutoCompleteRespV2"

newtype Suggestion = Suggestion
  { placePrediction :: PlacePrediction
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data PlacePrediction = PlacePrediction
  { text :: PlaceText,
    placeId :: Maybe Text,
    types :: Maybe [Text],
    distanceMeters :: Maybe Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

newtype PlaceText = PlaceText
  { text :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

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

-------------------------------------------------------------------------------
-- Google Geocoding API v4 : "Search for destinations"
--   POST https://geocode.googleapis.com/v4/geocode/destinations
--   Docs: https://developers.google.com/maps/documentation/geocoding/search-for-destinations
-------------------------------------------------------------------------------

-- | Aeson options for request bodies: drop unset (Nothing) fields so we never
-- serialise a null for the unused one-of query fields. The endpoint expects
-- exactly one of addressQuery / place / locationQuery.
searchDestinationsReqOptions :: Options
searchDestinationsReqOptions = defaultOptions {omitNothingFields = True}

-- | Request body. Provide EXACTLY ONE of addressQuery, place or locationQuery.
data SearchDestinationsReq = SearchDestinationsReq
  { -- | one-of #1: free-text or structured address
    addressQuery :: Maybe AddressQuery,
    -- | one-of #2: place resource name, e.g. "places/ChIJY8sv5-i2j4AR_S6BlDDR42w"
    place :: Maybe Text,
    -- | one-of #3: reverse lookup by lat/lng
    locationQuery :: Maybe LocationQuery,
    -- | IETF BCP-47 language code, e.g. "en"
    languageCode :: Maybe Text,
    -- | CLDR two-letter region code, e.g. "US"
    regionCode :: Maybe Text,
    -- | filter navigation points; only DRIVE / WALK are accepted
    travelModes :: Maybe [ModeV2],
    placeFilter :: Maybe PlaceFilter
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

instance ToJSON SearchDestinationsReq where toJSON = genericToJSON searchDestinationsReqOptions

instance FromJSON SearchDestinationsReq where parseJSON = genericParseJSON searchDestinationsReqOptions

-- | addressQuery has two mutually-exclusive shapes:
--   { "addressQuery": "601 S Bernardo Ave, Sunnyvale, CA 94087, USA" }  (unstructured)
--   { "address": { ...PostalAddress... } }                             (structured)
data AddressQuery = AddressQuery
  { addressQuery :: Maybe Text,
    address :: Maybe PostalAddress
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

instance ToJSON AddressQuery where toJSON = genericToJSON searchDestinationsReqOptions

instance FromJSON AddressQuery where parseJSON = genericParseJSON searchDestinationsReqOptions

newtype LocationQuery = LocationQuery
  { location :: LatLngV2
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PlaceFilter = PlaceFilter
  { structureType :: Maybe StructureType,
    -- | "PRIMARY" | "WEAK" | "ANY"
    addressability :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

instance ToJSON PlaceFilter where toJSON = genericToJSON searchDestinationsReqOptions

instance FromJSON PlaceFilter where parseJSON = genericParseJSON searchDestinationsReqOptions

data StructureType = POINT | SECTION | BUILDING | GROUNDS
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | google.type.PostalAddress (used both in the structured request and the response).
data PostalAddress = PostalAddress
  { revision :: Maybe Int,
    regionCode :: Maybe Text,
    languageCode :: Maybe Text,
    postalCode :: Maybe Text,
    sortingCode :: Maybe Text,
    administrativeArea :: Maybe Text,
    locality :: Maybe Text,
    sublocality :: Maybe Text,
    addressLines :: Maybe [Text],
    recipients :: Maybe [Text],
    organization :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToSchema)

instance ToJSON PostalAddress where toJSON = genericToJSON searchDestinationsReqOptions

instance FromJSON PostalAddress where parseJSON = genericParseJSON searchDestinationsReqOptions

data LocalizedText = LocalizedText
  { text :: Maybe Text,
    languageCode :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Response body.
newtype SearchDestinationsResp = SearchDestinationsResp
  { destinations :: Maybe [Destination]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Destination = Destination
  { -- | the main identified place
    primary :: Maybe DestinationPlace,
    -- | larger entities containing the primary destination
    containingPlaces :: Maybe [DestinationPlace],
    -- | specific locations within the primary destination
    subDestinations :: Maybe [DestinationPlace],
    entrances :: Maybe [Entrance],
    navigationPoints :: Maybe [NavigationPoint],
    landmarks :: Maybe [Landmark],
    -- | experimental (v4alpha)
    arrivalSummary :: Maybe LocalizedText,
    -- | experimental (v4alpha)
    parkingOptions :: Maybe Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | The core place object, reused by primary / containing / sub / landmark places.
data DestinationPlace = DestinationPlace
  { -- | "places/ChIJ..."
    place :: Maybe Text,
    displayName :: Maybe LocalizedText,
    primaryType :: Maybe Text,
    types :: Maybe [Text],
    formattedAddress :: Maybe Text,
    postalAddress :: Maybe PostalAddress,
    -- | "POINT" | "SECTION" | "BUILDING" | "GROUNDS" | "..._UNSPECIFIED".
    -- Kept as 'Text' (not the 'StructureType' enum) so an unknown/new value
    -- from Google does not fail the whole response decode.
    structureType :: Maybe Text,
    location :: Maybe LatLngV2,
    displayPolygon :: Maybe DisplayPolygon
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | GeoJSON geometry: { "type": "Polygon"|"MultiPolygon", "coordinates": ... }.
-- The leading underscore on @_type@ is stripped so the JSON key is "type".
data DisplayPolygon = DisplayPolygon
  { _type :: Maybe Text,
    -- | Raw GeoJSON coordinates. Kept as a 'Value' because the nesting depth
    -- varies by geometry type (Polygon = @[[[Double]]]@, MultiPolygon =
    -- @[[[[Double]]]]@); decoding to a fixed depth fails on the other shape.
    coordinates :: Maybe Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

instance ToJSON DisplayPolygon where toJSON = genericToJSON JS.stripPrefixUnderscoreIfAny

instance FromJSON DisplayPolygon where parseJSON = genericParseJSON JS.stripPrefixUnderscoreIfAny

data Entrance = Entrance
  { location :: Maybe LatLngV2,
    -- | e.g. ["PREFERRED"]
    entrance_tags :: Maybe [Text],
    place :: Maybe Text,
    streetViewThumbnail :: Maybe Value,
    streetViewAnnotation :: Maybe Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NavigationPoint = NavigationPoint
  { -- | token usable with the Routes API
    navigationPointToken :: Maybe Text,
    location :: Maybe LatLngV2,
    -- | ["DRIVE","WALK"] — 'Text' (not the 'ModeV2' enum) so an unknown/new
    -- travel mode from Google does not fail the whole response decode.
    travelModes :: Maybe [Text],
    -- | ["PARKING","PICKUP","DROPOFF"]
    usages :: Maybe [Text],
    -- | experimental (v4alpha)
    altitude_meters :: Maybe Double,
    streetViewThumbnail :: Maybe Value,
    entranceAnnotation :: Maybe Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Landmark = Landmark
  { place :: Maybe DestinationPlace,
    -- | e.g. "Near Chase Bank"
    relationalDescription :: Maybe LocalizedText,
    -- | ["ARRIVAL","ADDRESS"]
    tags :: Maybe [Text],
    straightLineDistanceMeters :: Maybe Double,
    travelDistanceMeters :: Maybe Double
  }
  deriving stock (Show, Generic)
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

data TransitPreferencesV2 = TransitPreferencesV2
  { allowedTravelModes :: Maybe [TransitTravelModeV2],
    routingPreference :: Maybe TransitRoutingPreferenceV2
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data TransitDirectionsReq = TransitDirectionsReq
  { origin :: WayPointV2,
    destination :: WayPointV2,
    routingPreference :: RoutingPreference,
    travelMode :: Maybe ModeV2,
    computeAlternativeRoutes :: Bool,
    transitPreferences :: Maybe TransitPreferencesV2,
    routeModifiers :: Maybe RouteModifiers,
    arrivalTime :: Maybe String, -- yyyy-mm-ddThh:mm:ssZ
    departureTime :: Maybe String
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

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
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype LocationV2 = LocationV2
  { latLng :: LatLngV2
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

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
    staticDuration :: Maybe Text,
    routeToken :: Maybe Text
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
    travelMode :: ModeV2,
    transitDetails :: Maybe TransitDetailsV2
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TransitDetailsV2 = TransitDetailsV2
  { transitLine :: TransitLineV2,
    stopDetails :: StopDetailsV2
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data StopDetailsV2 = StopDetailsV2
  { arrivalTime :: Text,
    departureTime :: Text,
    arrivalStop :: TransitStopV2,
    departureStop :: TransitStopV2
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data TransitStopV2 = TransitStopV2
  { name :: Text,
    location :: LocationV2
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data TransitLineV2 = TransitLineV2
  { agencies :: [AgencyV2],
    vehicle :: VehicleV2
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype AgencyV2 = AgencyV2
  { name :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype VehicleV2 = VehicleV2
  { _type :: TransitVehicleTypeV2
  }
  deriving (Generic, ToSchema)

instance FromJSON VehicleV2 where
  parseJSON = genericParseJSON JS.stripPrefixUnderscoreIfAny

instance ToJSON VehicleV2 where
  toJSON = genericToJSON JS.stripPrefixUnderscoreIfAny

newtype Polyline = Polyline
  { encodedPolyline :: PolyLinePoints
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

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

instance ToHttpApiData Mode where
  toUrlPiece = T.toLower . show

instance FromHttpApiData Mode where
  parseUrlPiece = left T.pack . eitherDecode .(\str -> "\"" <> str <> "\"") . BSL.fromStrict . DT.encodeUtf8 . T.toUpper

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

data TransitTravelModeV2
  = TRANSIT_TRAVEL_MODE_UNSPECIFIED
  | BUS
  | SUBWAY
  | TRAIN
  | LIGHT_RAIL
  | RAIL
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data TransitRoutingPreferenceV2
  = TRANSIT_ROUTING_PREFERENCE_UNSPECIFIED
  | LESS_WALKING
  | FEWER_TRANSFERS
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data TransitVehicleTypeV2
  = VEHICLE_TYPE_TRANSIT_VEHICLE_TYPE_UNSPECIFIED
  | VEHICLE_TYPE_BUS
  | VEHICLE_TYPE_CABLE_CAR
  | VEHICLE_TYPE_COMMUTER_TRAIN
  | VEHICLE_TYPE_FERRY
  | VEHICLE_TYPE_FUNICULAR
  | VEHICLE_TYPE_GONDOLA_LIFT
  | VEHICLE_TYPE_HEAVY_RAIL
  | VEHICLE_TYPE_HIGH_SPEED_TRAIN
  | VEHICLE_TYPE_INTERCITY_BUS
  | VEHICLE_TYPE_LONG_DISTANCE_TRAIN
  | VEHICLE_TYPE_METRO_RAIL
  | VEHICLE_TYPE_MONORAIL
  | VEHICLE_TYPE_OTHER
  | VEHICLE_TYPE_RAIL
  | VEHICLE_TYPE_SHARE_TAXI
  | VEHICLE_TYPE_SUBWAY
  | VEHICLE_TYPE_TRAM
  | VEHICLE_TYPE_TROLLEYBUS
  deriving (Eq, Show, Generic, ToSchema)

instance ToJSON TransitVehicleTypeV2 where
  toJSON = \case
    VEHICLE_TYPE_TRANSIT_VEHICLE_TYPE_UNSPECIFIED -> "TRANSIT_VEHICLE_TYPE_UNSPECIFIED"
    VEHICLE_TYPE_BUS -> "BUS"
    VEHICLE_TYPE_CABLE_CAR -> "CABLE_CAR"
    VEHICLE_TYPE_COMMUTER_TRAIN -> "COMMUTER_TRAIN"
    VEHICLE_TYPE_FERRY -> "FERRY"
    VEHICLE_TYPE_FUNICULAR -> "FUNICULAR"
    VEHICLE_TYPE_GONDOLA_LIFT -> "GONDOLA_LIFT"
    VEHICLE_TYPE_HEAVY_RAIL -> "HEAVY_RAIL"
    VEHICLE_TYPE_HIGH_SPEED_TRAIN -> "HIGH_SPEED_TRAIN"
    VEHICLE_TYPE_INTERCITY_BUS -> "INTERCITY_BUS"
    VEHICLE_TYPE_LONG_DISTANCE_TRAIN -> "LONG_DISTANCE_TRAIN"
    VEHICLE_TYPE_METRO_RAIL -> "METRO_RAIL"
    VEHICLE_TYPE_MONORAIL -> "MONORAIL"
    VEHICLE_TYPE_OTHER -> "OTHER"
    VEHICLE_TYPE_RAIL -> "RAIL"
    VEHICLE_TYPE_SHARE_TAXI -> "SHARE_TAXI"
    VEHICLE_TYPE_SUBWAY -> "SUBWAY"
    VEHICLE_TYPE_TRAM -> "TRAM"
    VEHICLE_TYPE_TROLLEYBUS -> "TROLLEYBUS"

instance FromJSON TransitVehicleTypeV2 where
  parseJSON = \case
    "TRANSIT_VEHICLE_TYPE_UNSPECIFIED" -> pure VEHICLE_TYPE_TRANSIT_VEHICLE_TYPE_UNSPECIFIED
    "BUS" -> pure VEHICLE_TYPE_BUS
    "CABLE_CAR" -> pure VEHICLE_TYPE_CABLE_CAR
    "COMMUTER_TRAIN" -> pure VEHICLE_TYPE_COMMUTER_TRAIN
    "FERRY" -> pure VEHICLE_TYPE_FERRY
    "FUNICULAR" -> pure VEHICLE_TYPE_FUNICULAR
    "GONDOLA_LIFT" -> pure VEHICLE_TYPE_GONDOLA_LIFT
    "HEAVY_RAIL" -> pure VEHICLE_TYPE_HEAVY_RAIL
    "HIGH_SPEED_TRAIN" -> pure VEHICLE_TYPE_HIGH_SPEED_TRAIN
    "INTERCITY_BUS" -> pure VEHICLE_TYPE_INTERCITY_BUS
    "LONG_DISTANCE_TRAIN" -> pure VEHICLE_TYPE_LONG_DISTANCE_TRAIN
    "METRO_RAIL" -> pure VEHICLE_TYPE_METRO_RAIL
    "MONORAIL" -> pure VEHICLE_TYPE_MONORAIL
    "OTHER" -> pure VEHICLE_TYPE_OTHER
    "RAIL" -> pure VEHICLE_TYPE_RAIL
    "SHARE_TAXI" -> pure VEHICLE_TYPE_SHARE_TAXI
    "SUBWAY" -> pure VEHICLE_TYPE_SUBWAY
    "TRAM" -> pure VEHICLE_TYPE_TRAM
    "TROLLEYBUS" -> pure VEHICLE_TYPE_TROLLEYBUS
    v -> fail $ "Invalid TransitVehicleTypeV2: " <> show v

data DepartureTime = Now | FutureTime UTCTime

instance ToHttpApiData DepartureTime where
  toUrlPiece Now = "now"
  toUrlPiece (FutureTime time) = show time
