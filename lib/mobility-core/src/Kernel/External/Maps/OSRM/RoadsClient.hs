{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Kernel.External.Maps.OSRM.RoadsClient where

import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import Data.OpenApi (NamedSchema (..), ToSchema (..), declareSchema, fromAesonOptions, genericDeclareNamedSchema)
import qualified Data.Text as T
import qualified Data.Vector as V
import EulerHS.Prelude ((...))
import qualified EulerHS.Types as Euler
import qualified Kernel.External.Maps.Interface.Types as GetDistancesReq (GetDistancesReq (..))
import qualified Kernel.External.Maps.Interface.Types as GetRoutesReq (GetRoutesReq (..))
import qualified Kernel.External.Maps.Interface.Types as Maps
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude hiding (unlines)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Kernel.Utils.ExternalAPICallLogging as ApiCallLogger
import Kernel.Utils.GenericPretty
import Kernel.Utils.JSON (camelToSnakeCase, removeNullFields)
import Servant hiding (throwError)
import Servant.Client (ClientError (..), ResponseF (..))

-- types

data OSRMProfile = Driving | Walking | Bicycling
  deriving (Show, Eq)

instance ToHttpApiData OSRMProfile where
  toUrlPiece Driving = "driving"
  toUrlPiece Walking = "foot"
  toUrlPiece Bicycling = "bicycle"

-- Convert from Maps.TravelMode to OSRMProfile
toOSRMProfile :: Maps.TravelMode -> OSRMProfile
toOSRMProfile = \case
  Maps.CAR -> Driving
  Maps.MOTORCYCLE -> Driving
  Maps.BICYCLE -> Bicycling
  Maps.FOOT -> Walking

-- Helper functions for updating travel mode in requests
updateModeForGetDistancesReq :: Maps.TravelMode -> GetDistancesReq.GetDistancesReq a b -> GetDistancesReq.GetDistancesReq a b
updateModeForGetDistancesReq mode req = req {GetDistancesReq.travelMode = Just mode}

updateModeForGetRoutesReq :: Maps.TravelMode -> GetRoutesReq.GetRoutesReq -> GetRoutesReq.GetRoutesReq
updateModeForGetRoutesReq mode req = req {GetRoutesReq.mode = Just mode}

type MatchAPI =
  "match"
    :> "v1"
    :> Capture "profile" OSRMProfile
    :> Capture "coordinates" PointsList
    :> MandatoryQueryParam "overview" Text
    :> MandatoryQueryParam "steps" AlwaysTrue
    :> QueryParam "radiuses" RadiusesList
    :> MandatoryQueryParam "geometries" GeometryRespType
    :> Get '[JSON] MatchResp

type TableAPI =
  "table"
    :> "v1"
    :> Capture "profile" OSRMProfile
    :> Capture "coordinates" PointsList
    :> MandatoryQueryParam "annotations" String
    :> MandatoryQueryParam "sources" SourcesList
    :> MandatoryQueryParam "destinations" DestinationsList
    :> QueryParam "source_destination_mapping" Maps.SourceDestinationMapping
    :> Get '[JSON] OSRMTableResponse

type RouteAPI =
  "route"
    :> "v1"
    :> Capture "profile" OSRMProfile
    :> Capture "coordinates" PointsList
    :> MandatoryQueryParam "geometries" GeometryRespType
    :> MandatoryQueryParam "alternatives" Bool
    :> MandatoryQueryParam "steps" Bool
    :> QueryParam "overview" OverviewType
    :> QueryParam "annotations" AnnotationsList
    :> Get '[JSON] OSRMRouteResponse

type NearestAPI =
  "nearest"
    :> "v1"
    :> Capture "profile" OSRMProfile
    :> Capture "coordinates" PointsList
    :> QueryParam "number" Int
    :> Get '[JSON] OSRMNearestResponse

newtype PointsList = PointsList {getPointsList :: [Maps.LatLong]}

newtype SourcesList = SourcesList {getSourcesList :: [Int]}

newtype DestinationsList = DestinationsList {getDestinationsList :: [Int]}

instance ToHttpApiData PointsList where
  toUrlPiece (PointsList lst) = T.intercalate ";" $ map convertPoint lst

convertPoint :: Maps.LatLong -> Text
convertPoint (Maps.LatLong lat lon) = show lon <> "," <> show lat

data AlwaysTrue = AlwaysTrue

instance ToHttpApiData AlwaysTrue where
  toUrlPiece _ = "true"

newtype RadiusesList = RadiusesList {getRadiusesList :: [Int]}

instance ToHttpApiData RadiusesList where
  toUrlPiece (RadiusesList lst) = T.intercalate ";" $ map show lst

instance ToHttpApiData DestinationsList where
  toUrlPiece (DestinationsList lst) = T.intercalate ";" $ map show lst

instance ToHttpApiData SourcesList where
  toUrlPiece (SourcesList lst) = T.intercalate ";" $ map show lst

data GeometryRespType = GeoJson

instance ToHttpApiData GeometryRespType where
  toUrlPiece GeoJson = "geojson"

data OverviewType = OverviewFull | OverviewSimplified

instance ToHttpApiData OverviewType where
  toUrlPiece OverviewFull = "full"
  toUrlPiece OverviewSimplified = "simplified"

newtype AnnotationsList = AnnotationsList {getAnnotationsList :: [Text]}

instance ToHttpApiData AnnotationsList where
  toUrlPiece (AnnotationsList lst) = T.intercalate "," lst

data OSRMTableResponse = OSRMTableResponse
  { distances :: [[Double]],
    code :: String,
    durations :: [[Double]]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data MatchResp = MatchResp
  { matchings :: NonEmpty Route,
    tracepoints :: [Maybe Waypoint]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)

data Route = Route
  { distance :: Double, -- meters
    duration :: Double, -- seconds
    legs :: [RouteLeg],
    confidence :: Double, -- from 0 to 1, 1 is very confident
    geometry :: RouteGeometry
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, PrettyShow)

newtype RouteGeometry = RouteGeometry
  { coordinates :: [Location]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)

data RouteLeg = RouteLeg
  { distance :: Double, -- meters
    duration :: Double -- seconds
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, PrettyShow)

data Waypoint = Waypoint
  { alternatives_count :: Int,
    waypoint_index :: Int,
    matchings_index :: Int,
    location :: Location
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, PrettyShow)

newtype Location = Location {getLatLong :: Maps.LatLong}
  deriving stock (Generic, Show, Eq)
  deriving (PrettyShow) via (Showable Location)

data OSRMRouteResponse = OSRMRouteResponse
  { routes :: [OSRMRouteRoutes],
    waypoints :: [OSRMRouteWaypoint]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data OSRMRouteRoutes = OSRMRouteRoutes
  { geometry :: RouteGeometry,
    legs :: [RouteResponseLeg],
    distance :: Double, -- meters
    duration :: Double, -- seconds
    weight :: Double
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data RouteResponseLeg = RouteResponseLeg
  { steps :: [Steps],
    distance :: Double,
    duration :: Double,
    weight :: Double,
    annotation :: Maybe RouteAnnotation
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Per-segment measurements for one leg.
data RouteAnnotation = RouteAnnotation
  { speed :: Maybe [Double],
    distance :: Maybe [Double],
    duration :: Maybe [Double]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data Steps = Steps
  { geometry :: RouteGeometry,
    maneuver :: Maneuver,
    intersections :: Maybe [Intersection] -- Only when steps = true
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | A junction within a step.
data Intersection = Intersection
  { location :: Location,
    classes :: Maybe [Text]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

newtype Maneuver = Maneuver
  { location :: Location
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data OSRMRouteWaypoint = OSRMRouteWaypoint
  { distance :: Double,
    location :: Location
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data OSRMNearestResponse = OSRMNearestResponse
  { code :: Text,
    waypoints :: [OSRMNearestWaypoint]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data OSRMNearestWaypoint = OSRMNearestWaypoint
  { name :: Maybe Text,
    distance :: Double,
    location :: Location
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

instance FromJSON Location where
  parseJSON = withArray "array [lon, lat]" $ \arr_ -> case toList arr_ of
    [lonVal, latVal] -> Location ... Maps.LatLong <$> parseJSON latVal <*> parseJSON lonVal
    _ -> fail "expected array [lon, lat]"

instance ToJSON Location where
  toJSON (Location Maps.LatLong {..}) = Array $ V.fromList $ map toJSON [lon, lat]

-- | @[longitude, latitude]@, as OSRM writes a point.
instance ToSchema Location where
  declareNamedSchema _ = NamedSchema (Just "OSRMLocation") <$> declareSchema (Proxy @[Double])

-- call
callOsrmMatchAPI ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r,
    HasRequestId r
  ) =>
  Maybe Text ->
  Maybe Text ->
  Maps.SnapToRoadReq ->
  BaseUrl ->
  Maybe Int ->
  Maps.TravelMode -> -- Changed from Text to TravelMode
  PointsList ->
  m MatchResp
callOsrmMatchAPI entityId merchantCityId req osrmUrl mbRadius travelMode pointsList = do
  let pointsNum = length pointsList.getPointsList
      radiuses = flip fmap mbRadius $ \r -> RadiusesList $ replicate pointsNum r
      profile = toOSRMProfile travelMode
  let eulerClient = Euler.client (Proxy @MatchAPI)
  rsp <- callAPI osrmUrl (eulerClient profile pointsList "full" AlwaysTrue radiuses GeoJson) "osrm-match" (Proxy @MatchAPI)
  fork ("Logging external API Call of OsrmMatchAPI OSRM ") $
    ApiCallLogger.pushExternalApiCallDataToKafka "OsrmMatchAPI" "OSRM" entityId merchantCityId (Just req) rsp
  fromEitherM (FailedToCallOsrmMatchAPI . show) rsp

getResultOneRouteExpected :: (Log m, MonadThrow m) => MatchResp -> m (HighPrecMeters, Double, [Maps.LatLong])
getResultOneRouteExpected resp = do
  logDebug $ T.pack $ defaultPretty resp
  route_ <- case NE.tail resp.matchings of
    [] -> pure $ NE.head resp.matchings
    (_ : _) -> throwError $ InternalError "OSRM failed to consider waypoints as part of one route, th result contains splitted routes"
  let points = map (.getLatLong) $ route_.geometry.coordinates
  pure (realToFrac route_.distance, route_.confidence, points)

callOsrmGetDistancesAPI ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r,
    ToJSON a,
    ToJSON b,
    HasRequestId r,
    MonadReader r m
  ) =>
  Maybe Text ->
  Maybe Text ->
  Maps.GetDistancesReq a b ->
  BaseUrl ->
  Maps.TravelMode -> -- Changed from Text to TravelMode
  PointsList ->
  SourcesList ->
  DestinationsList ->
  Maybe Maps.SourceDestinationMapping ->
  m OSRMTableResponse
callOsrmGetDistancesAPI entityId merchantCityId req osrmUrl travelMode pointsList sourcesList destinationsList mbSourceDestinationMapping =
  do
    let eulerClient = Euler.client (Proxy @TableAPI)
        profile = toOSRMProfile travelMode
        updatedReq = updateModeForGetDistancesReq travelMode req
    rsp <- callAPI osrmUrl (eulerClient profile pointsList "distance,duration" sourcesList destinationsList mbSourceDestinationMapping) "osrm-table" (Proxy @TableAPI)
    fork ("Logging external API Call of OsrmGetDistancesAPI OSRM ") $
      ApiCallLogger.pushExternalApiCallDataToKafka "OsrmGetDistancesAPI" "OSRM" entityId merchantCityId (Just updatedReq) rsp
    fromEitherM (FailedToCallOsrmTableAPI . show) rsp

callOsrmRouteAPI ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r,
    HasRequestId r,
    MonadReader r m
  ) =>
  Maybe Text ->
  Maybe Text ->
  Maps.GetRoutesReq ->
  BaseUrl ->
  Maps.TravelMode -> -- Changed from Text to TravelMode
  PointsList ->
  Bool ->
  m OSRMRouteResponse
callOsrmRouteAPI entityId merchantCityId req osrmUrl travelMode pointsList withTrafficAnnotations = do
  let eulerClient = Euler.client (Proxy @RouteAPI)
      profile = toOSRMProfile travelMode
      updatedReq = updateModeForGetRoutesReq travelMode req
      (mbOverview, mbAnnotations) =
        if withTrafficAnnotations
          then (Just OverviewFull, Just $ AnnotationsList ["speed", "distance"])
          else (Nothing, Nothing)
  rsp <- callAPI osrmUrl (eulerClient profile pointsList GeoJson True True mbOverview mbAnnotations) "osrm-route" (Proxy @RouteAPI)
  fork ("Logging external API Call of OsrmRouteAPI OSRM ") $
    ApiCallLogger.pushExternalApiCallDataToKafka "OsrmRouteAPI" "OSRM" entityId merchantCityId (Just updatedReq) rsp
  fromEitherM (FailedToCallOsrmRouteAPI . show) rsp

callOsrmNearestAPI ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r,
    HasRequestId r
  ) =>
  Maybe Text ->
  Maybe Text ->
  Maps.GetNearestPointsReq ->
  BaseUrl ->
  Maps.TravelMode ->
  PointsList ->
  Maybe Int ->
  m OSRMNearestResponse
callOsrmNearestAPI entityId merchantCityId req osrmUrl travelMode pointsList mbNumber = do
  let eulerClient = Euler.client (Proxy @NearestAPI)
      profile = toOSRMProfile travelMode
  rsp <- callAPI osrmUrl (eulerClient profile pointsList mbNumber) "osrm-nearest" (Proxy @NearestAPI)
  fork ("Logging external API Call of OsrmNearestAPI OSRM ") $
    ApiCallLogger.pushExternalApiCallDataToKafka "OsrmNearestAPI" "OSRM" entityId merchantCityId (Just req) rsp
  fromEitherM (FailedToCallOsrmNearestAPI . show) rsp

-- navigation

-- OSRM routes for turn-by-turn navigation. 'callOsrmRouteAPI' parses OSRM's answer into
-- 'OSRMRouteResponse', which keeps only each step's location: the turn type, direction and road
-- name that a navigation engine needs are dropped. 'getOsrmNavigationRoute' asks OSRM for every
-- step, the full polyline6 geometry and per-segment annotations, and returns them as
-- 'NavigationRouteResp': only the fields a navigation engine (e.g. MapLibre Navigation) reads, under
-- OSRM's own names, so the engine can read the response as is.

-- | OSRM's route endpoint with every option navigation needs.
type NavigationRouteAPI =
  "route"
    :> "v1"
    :> Capture "profile" OSRMProfile
    :> Capture "coordinates" PointsList
    :> MandatoryQueryParam "steps" Text
    :> MandatoryQueryParam "overview" Text
    :> MandatoryQueryParam "geometries" Text
    :> MandatoryQueryParam "annotations" Text
    :> MandatoryQueryParam "alternatives" Text
    :> MandatoryQueryParam "continue_straight" Text
    :> QueryParam "bearings" Text
    :> QueryParam "exclude" Text
    :> QueryParam "waypoints" Text
    :> QueryParam "radiuses" Text
    :> Get '[JSON] OsrmNavigationRouteResp

-- | A route for turn-by-turn navigation: OSRM's route with only the fields a navigation engine reads,
-- plus @tollsAvoided@, true only when the route was computed with tolls excluded, and
-- @followsSearchRoute@, true only when the route was computed through the 'viaPoints'.
data NavigationRouteResp = NavigationRouteResp
  { code :: Text,
    routes :: [NavigationRoute],
    tollsAvoided :: Bool,
    followsSearchRoute :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | OSRM's answer to 'NavigationRouteAPI'. Fields the navigation types don't declare (weights,
-- intersection bearings, waypoint hints, ...) are dropped while decoding.
data OsrmNavigationRouteResp = OsrmNavigationRouteResp
  { code :: Text,
    routes :: [NavigationRoute]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data NavigationRoute = NavigationRoute
  { -- | The whole route as a polyline6 line.
    geometry :: Text,
    distance :: Double, -- meters
    duration :: Double, -- seconds

    -- | One leg per stretch between two stops.
    legs :: [NavigationRouteLeg]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data NavigationRouteLeg = NavigationRouteLeg
  { distance :: Double, -- meters
    duration :: Double, -- seconds
    steps :: [NavigationStep],
    annotation :: Maybe NavigationAnnotation
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON)

instance ToJSON NavigationRouteLeg where
  toJSON = genericToJSON removeNullFields

instance ToSchema NavigationRouteLeg where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions removeNullFields)

-- | Values for each segment of a leg (the line between two consecutive points of its geometry), in
-- travel order. @nodes@ has one entry more than the others: segment @i@ runs from OSM node @nodes[i]@
-- to @nodes[i + 1]@.
data NavigationAnnotation = NavigationAnnotation
  { distance :: Maybe [Double], -- meters
    duration :: Maybe [Double], -- seconds
    speed :: Maybe [Double], -- meters per second
    nodes :: Maybe [Integer]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON)

instance ToJSON NavigationAnnotation where
  toJSON = genericToJSON removeNullFields

instance ToSchema NavigationAnnotation where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions removeNullFields)

-- | One manoeuvre and the road up to the next one.
data NavigationStep = NavigationStep
  { -- | This step's part of the route as a polyline6 line.
    geometry :: Text,
    distance :: Double, -- meters
    duration :: Double, -- seconds

    -- | The road the step is on.
    name :: Maybe Text,
    maneuver :: NavigationManeuver,
    -- | Never empty: the navigation engine reads the first intersection of every step.
    intersections :: NonEmpty NavigationIntersection
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON)

instance ToJSON NavigationStep where
  toJSON = genericToJSON removeNullFields

instance ToSchema NavigationStep where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions removeNullFields)

data NavigationManeuver = NavigationManeuver
  { location :: Location,
    bearingBefore :: Double, -- degrees
    bearingAfter :: Double, -- degrees

    -- | OSRM's @type@, e.g. @turn@, @roundabout@, @arrive@.
    maneuverType :: Text,
    -- | e.g. @left@, @slight right@, @uturn@.
    modifier :: Maybe Text,
    -- | The exit to take, on roundabouts.
    exit :: Maybe Int
  }
  deriving stock (Generic, Show, Eq)

-- | OSRM's names: @bearing_before@, @bearing_after@ and @type@.
maneuverJsonOptions :: Options
maneuverJsonOptions =
  removeNullFields
    { fieldLabelModifier = \case
        "maneuverType" -> "type"
        field -> camelToSnakeCase field
    }

instance FromJSON NavigationManeuver where
  parseJSON = genericParseJSON maneuverJsonOptions

instance ToJSON NavigationManeuver where
  toJSON = genericToJSON maneuverJsonOptions

instance ToSchema NavigationManeuver where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions maneuverJsonOptions)

newtype NavigationIntersection = NavigationIntersection
  { location :: Location
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data NavigationRouteOptions = NavigationRouteOptions
  { -- | The vehicle's heading in degrees, when known. OSRM then starts the route in that direction,
    -- which avoids a route that begins with a U-turn behind the vehicle after a reroute.
    originBearing :: Maybe Double,
    -- | Prefer a route without toll roads.
    avoidTolls :: Bool,
    -- | Points to drive through on the way to the destination, in travel order (for example some points
    -- of a route quoted earlier, so the vehicle takes the same roads). OSRM passes through them without
    -- treating them as stops; when they don't fit the road network the route is made without them.
    viaPoints :: [Maps.LatLong]
  }
  deriving (Generic, Show, ToJSON)

type NavigationRouteFlow m r =
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r,
    HasRequestId r
  )

-- | OSRM's route from @origin@ through the 'viaPoints' and @waypoints@ (the last waypoint is the
-- destination), with every step (see 'NavigationRouteResp').
--
-- When tolls are to be avoided, the route is fetched again without the exclusion in two cases only:
-- no toll-free route exists, or the OSRM profile has no toll class. The route then carries
-- @tollsAvoided: false@, so the app can tell the driver it uses tolls instead of giving them a toll
-- route silently.
--
-- When OSRM refuses the 'viaPoints' because they don't fit the road network (a point with no road
-- nearby, or points that can't be connected), the route is fetched again without them and carries
-- @followsSearchRoute: false@. Any other failure (timeout, server error) is thrown without a retry.
getOsrmNavigationRoute ::
  NavigationRouteFlow m r =>
  Maybe Text ->
  Maybe Text ->
  BaseUrl ->
  Maps.TravelMode ->
  Maps.LatLong ->
  [Maps.LatLong] ->
  NavigationRouteOptions ->
  m NavigationRouteResp
getOsrmNavigationRoute entityId merchantCityId osrmUrl travelMode origin waypoints options = do
  let callOsrm viaPoints = callOsrmNavigationRouteAPI entityId merchantCityId osrmUrl travelMode origin viaPoints waypoints options.originBearing
      routeAvoidingTolls viaPoints = do
        firstTry <- callOsrm viaPoints options.avoidTolls
        case firstTry of
          Left err
            | options.avoidTolls,
              Just reason <- tollFallbackReason err -> do
              logInfo $ "OSRM navigation route fetched without excluding tolls: " <> reason
              (,False) <$> callOsrm viaPoints False
          _ -> pure (firstTry, options.avoidTolls)
  guided <- case options.viaPoints of
    [] -> pure Nothing
    viaPoints ->
      routeAvoidingTolls viaPoints >>= \case
        (Left err, _)
          | Just reason <- guideFallbackReason err -> do
            logInfo $ "OSRM navigation route fetched without its guide points: " <> reason
            pure Nothing
        result -> pure (Just result)
  (rsp, tollsAvoided) <- maybe (routeAvoidingTolls []) pure guided
  navigationRouteResp tollsAvoided (isJust guided) <$> fromEitherM (FailedToCallOsrmRouteAPI . show) rsp

-- | What is logged to Kafka for each call.
data NavigationRouteLog = NavigationRouteLog
  { origin :: Maps.LatLong,
    waypoints :: [Maps.LatLong],
    viaPointCount :: Int,
    originBearing :: Maybe Double,
    excludeTolls :: Bool
  }
  deriving (Generic, Show, ToJSON)

-- | One call to OSRM, logged to Kafka; the result is returned so the caller can inspect a refusal.
callOsrmNavigationRouteAPI ::
  NavigationRouteFlow m r =>
  Maybe Text ->
  Maybe Text ->
  BaseUrl ->
  Maps.TravelMode ->
  Maps.LatLong ->
  [Maps.LatLong] ->
  [Maps.LatLong] ->
  Maybe Double ->
  Bool ->
  m (Either ClientError OsrmNavigationRouteResp)
callOsrmNavigationRouteAPI entityId merchantCityId osrmUrl travelMode origin viaPoints waypoints originBearing excludeTolls = do
  let eulerClient = Euler.client (Proxy @NavigationRouteAPI)
      profile = toOSRMProfile travelMode
      coordinates = navigationCoordinates origin viaPoints waypoints originBearing
      exclude = if excludeTolls then Just "toll" else Nothing
      viaPointCount = length viaPoints
  rsp <-
    callAPI
      osrmUrl
      (eulerClient profile (PointsList coordinates.routeCoordinates) "true" "full" "polyline6" "distance,duration,speed,nodes" "false" "true" coordinates.bearingValues exclude coordinates.stopIndices coordinates.radiusValues)
      "osrm-navigation-route"
      (Proxy @NavigationRouteAPI)
  fork "Logging external API Call of OsrmNavigationRouteAPI OSRM" $
    ApiCallLogger.pushExternalApiCallDataToKafka "OsrmNavigationRouteAPI" "OSRM" entityId merchantCityId (Just NavigationRouteLog {..}) rsp
  pure rsp

-- | The coordinates OSRM is asked to route through, with the query parameters that depend on them.
data NavigationCoordinates = NavigationCoordinates
  { -- | The origin, the guide points, then the waypoints.
    routeCoordinates :: [Maps.LatLong],
    -- | Which coordinates are stops (the origin and the waypoints): OSRM treats every coordinate as a
    -- stop unless told otherwise, and would announce an arrival at each guide point. Absent without guide points.
    stopIndices :: Maybe Text,
    bearingValues :: Maybe Text,
    -- | How far from its coordinate OSRM may look for a road: bounded for the guide points, so a point
    -- with no road nearby is refused instead of snapped to a distant road. Absent without guide points.
    radiusValues :: Maybe Text
  }
  deriving (Show, Eq)

navigationCoordinates :: Maps.LatLong -> [Maps.LatLong] -> [Maps.LatLong] -> Maybe Double -> NavigationCoordinates
navigationCoordinates origin viaPoints waypoints originBearing =
  NavigationCoordinates
    { routeCoordinates = origin : viaPoints <> waypoints,
      stopIndices = onlyWithGuidePoints $ T.intercalate ";" $ map show $ 0 : [viaCount + 1 .. viaCount + length waypoints],
      bearingValues = bearingsParam (viaCount + length waypoints) <$> originBearing,
      radiusValues = onlyWithGuidePoints $ T.intercalate ";" $ "unlimited" : replicate viaCount (show guidePointSnapRadiusMeters) <> replicate (length waypoints) "unlimited"
    }
  where
    viaCount = length viaPoints
    onlyWithGuidePoints value = if viaCount == 0 then Nothing else Just value

-- | How far from a guide point (in metres) OSRM may look for a road: a little more than the gap between
-- two routing providers' versions of the same road, but not enough to reach a different road.
guidePointSnapRadiusMeters :: Int
guidePointSnapRadiusMeters = 30

-- | OSRM's @bearings@ value: the heading (with a tolerance) for the origin, none for the other coordinates.
bearingsParam :: Int -> Double -> Text
bearingsParam otherCoordinateCount heading =
  T.intercalate ";" $ (show (round heading `mod` 360 :: Int) <> "," <> show bearingToleranceDegrees) : replicate otherCoordinateCount ""

-- | How far (in degrees) OSRM may deviate from the heading when choosing the start road.
bearingToleranceDegrees :: Int
bearingToleranceDegrees = 45

-- | OSRM's error body, e.g. @{"code": "NoRoute", "message": "Impossible route between points"}@.
data OsrmErrorBody = OsrmErrorBody Text (Maybe Text)

instance A.FromJSON OsrmErrorBody where
  parseJSON = A.withObject "OsrmErrorBody" $ \o -> OsrmErrorBody <$> o A..: "code" <*> o A..:? "message"

-- | Why a route with tolls excluded failed, when fetching it again with tolls allowed is the right
-- fallback; 'Nothing' for any other failure. OSRM answers every error with HTTP 400, so the body's
-- code tells them apart: @NoRoute@ when no toll-free route exists, and @InvalidValue@ with an
-- "Exclude flag combination is not supported." message when the profile has no toll class.
tollFallbackReason :: ClientError -> Maybe Text
tollFallbackReason = \case
  FailureResponse _ Response {responseBody} -> case A.decode responseBody of
    Just (OsrmErrorBody "NoRoute" _) -> Just "no toll-free route exists"
    Just (OsrmErrorBody "InvalidValue" (Just message))
      | "Exclude" `T.isInfixOf` message -> Just "the OSRM profile has no toll class"
    _ -> Nothing
  _ -> Nothing

-- | The OSRM error code when OSRM refused a route through guide points because they don't fit the road
-- network, so that fetching the route again without them is the right fallback; 'Nothing' for any other
-- failure. @NoSegment@: a guide point has no road within its snap radius. @NoRoute@: the points can't be connected.
guideFallbackReason :: ClientError -> Maybe Text
guideFallbackReason = \case
  FailureResponse _ Response {responseBody} -> guideFallbackReasonFromBody responseBody
  _ -> Nothing

-- | 'guideFallbackReason' for an OSRM error response body.
guideFallbackReasonFromBody :: LBS.ByteString -> Maybe Text
guideFallbackReasonFromBody body = case A.decode body of
  Just (OsrmErrorBody code _) | code `elem` ["NoSegment", "NoRoute"] -> Just code
  _ -> Nothing

-- | OSRM's route with @tollsAvoided@ and @followsSearchRoute@ added.
navigationRouteResp :: Bool -> Bool -> OsrmNavigationRouteResp -> NavigationRouteResp
navigationRouteResp tollsAvoided followsSearchRoute OsrmNavigationRouteResp {code, routes} = NavigationRouteResp {..}
