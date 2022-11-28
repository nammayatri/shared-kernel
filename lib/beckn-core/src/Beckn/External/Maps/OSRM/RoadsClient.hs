{-# LANGUAGE DerivingVia #-}

module Beckn.External.Maps.OSRM.RoadsClient where

import qualified Beckn.External.Maps.Types as Maps
import Beckn.Prelude hiding (unlines)
import qualified Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Utils.Common
import Beckn.Utils.GenericPretty
import Data.Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector as V
import EulerHS.Prelude ((...))
import qualified EulerHS.Types as Euler
import Servant hiding (throwError)

-- types

type MatchAPI =
  "match"
    :> "v1"
    :> "car"
    :> Capture "coordinates" PointsList
    :> MandatoryQueryParam "tidy" AlwaysTrue
    :> QueryParam "radiuses" RadiusesList
    :> MandatoryQueryParam "geometries" GeometryRespType
    :> Get '[JSON] MatchResp

newtype PointsList = PointsList {getPointsList :: [Maps.LatLong]}

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

data GeometryRespType = GeoJson

instance ToHttpApiData GeometryRespType where
  toUrlPiece GeoJson = "geojson"

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

instance FromJSON Location where
  parseJSON = withArray "array [lon, lat]" $ \arr_ -> case toList arr_ of
    [lonVal, latVal] -> Location ... Maps.LatLong <$> parseJSON latVal <*> parseJSON lonVal
    _ -> fail "expected array [lon, lat]"

instance ToJSON Location where
  toJSON (Location Maps.LatLong {..}) = Array $ V.fromList $ map toJSON [lon, lat]

-- call
callOsrmMatchAPI ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Maybe Int ->
  PointsList ->
  m MatchResp
callOsrmMatchAPI osrmUrl mbRadius pointsList = do
  let pointsNum = length pointsList.getPointsList
      radiuses = flip fmap mbRadius $ \r -> RadiusesList $ replicate pointsNum r
  let eulerClient = Euler.client (Proxy @MatchAPI)
  callAPI osrmUrl (eulerClient pointsList AlwaysTrue radiuses GeoJson) "osrm-match"
    >>= fromEitherM (\err -> InternalError $ "Failed to call osrm match API: " <> show err)

getResultOneRouteExpected :: (Log m, MonadThrow m) => MatchResp -> m (HighPrecMeters, [Maps.LatLong])
getResultOneRouteExpected resp = do
  logDebug $ T.pack $ defaultPretty resp
  route_ <- case NE.tail resp.matchings of
    [] -> pure $ NE.head resp.matchings
    (_ : _) -> throwError $ InternalError "OSRM failed to consider waypoints as part of one route, th result contains splitted routes"
  let points = map (.getLatLong) $ route_.geometry.coordinates
  pure (realToFrac route_.distance, points)
