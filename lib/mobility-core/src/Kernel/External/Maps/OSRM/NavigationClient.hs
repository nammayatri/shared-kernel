{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | OSRM routes for turn-by-turn navigation.
--
-- 'Kernel.External.Maps.OSRM.RoadsClient.callOsrmRouteAPI' parses OSRM's answer into
-- 'OSRMRouteResponse', which keeps only each step's location: the turn type, direction and road
-- name that a navigation engine needs are dropped. This client asks OSRM for every step, the full
-- polyline6 geometry and per-segment annotations, and returns OSRM's response as raw JSON so a
-- navigation engine (e.g. MapLibre Navigation) can read it as is.
module Kernel.External.Maps.OSRM.NavigationClient
  ( NavigationRouteAPI,
    NavigationRouteOptions (..),
    getOsrmNavigationRoute,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified EulerHS.Types as Euler
import qualified Kernel.External.Maps.Interface.Types as MapsInterface
import Kernel.External.Maps.OSRM.RoadsClient (OSRMProfile, PointsList (..), toOSRMProfile)
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Kernel.Utils.ExternalAPICallLogging as ApiCallLogger
import Servant hiding (throwError)
import Servant.Client (ClientError (..), ResponseF (..))

-- | OSRM's route endpoint with every option navigation needs; the response is passed through as raw JSON.
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
    :> Get '[JSON] Value

data NavigationRouteOptions = NavigationRouteOptions
  { -- | The vehicle's heading in degrees, when known. OSRM then starts the route in that direction,
    -- which avoids a route that begins with a U-turn behind the vehicle after a reroute.
    originBearing :: Maybe Double,
    -- | Prefer a route without toll roads.
    avoidTolls :: Bool
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

-- | OSRM's route from @origin@ through @waypoints@ (the last one is the destination), with every
-- step, plus one added field: @tollsAvoided@, true only when the route was computed with tolls
-- excluded.
--
-- When tolls are to be avoided, the route is fetched again without the exclusion in two cases only:
-- no toll-free route exists, or the OSRM profile has no toll class. The route then carries
-- @tollsAvoided: false@, so the app can tell the driver it uses tolls instead of giving them a toll
-- route silently. Any other failure (timeout, server error) is thrown without a retry.
getOsrmNavigationRoute ::
  NavigationRouteFlow m r =>
  Maybe Text ->
  Maybe Text ->
  BaseUrl ->
  MapsInterface.TravelMode ->
  Maps.LatLong ->
  [Maps.LatLong] ->
  NavigationRouteOptions ->
  m Value
getOsrmNavigationRoute entityId merchantCityId osrmUrl travelMode origin waypoints options = do
  let callOsrm = callOsrmNavigationRouteAPI entityId merchantCityId osrmUrl travelMode origin waypoints options.originBearing
  firstTry <- callOsrm options.avoidTolls
  (rsp, tollsAvoided) <- case firstTry of
    Left err
      | options.avoidTolls,
        Just reason <- tollFallbackReason err -> do
        logInfo $ "OSRM navigation route fetched without excluding tolls: " <> reason
        (,False) <$> callOsrm False
    _ -> pure (firstTry, options.avoidTolls)
  withTollsAvoided tollsAvoided <$> fromEitherM (FailedToCallOsrmRouteAPI . show) rsp

-- | What is logged to Kafka for each call.
data NavigationRouteLog = NavigationRouteLog
  { origin :: Maps.LatLong,
    waypoints :: [Maps.LatLong],
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
  MapsInterface.TravelMode ->
  Maps.LatLong ->
  [Maps.LatLong] ->
  Maybe Double ->
  Bool ->
  m (Either ClientError Value)
callOsrmNavigationRouteAPI entityId merchantCityId osrmUrl travelMode origin waypoints originBearing excludeTolls = do
  let eulerClient = Euler.client (Proxy @NavigationRouteAPI)
      profile = toOSRMProfile travelMode
      points = PointsList (origin : waypoints)
      bearings = bearingsParam (length waypoints) <$> originBearing
      exclude = if excludeTolls then Just "toll" else Nothing
  rsp <-
    callAPI
      osrmUrl
      (eulerClient profile points "true" "full" "polyline6" "distance,duration,speed" "false" "true" bearings exclude)
      "osrm-navigation-route"
      (Proxy @NavigationRouteAPI)
  fork "Logging external API Call of OsrmNavigationRouteAPI OSRM" $
    ApiCallLogger.pushExternalApiCallDataToKafka "OsrmNavigationRouteAPI" "OSRM" entityId merchantCityId (Just NavigationRouteLog {..}) rsp
  pure rsp

-- | OSRM's @bearings@ value: the heading (with a tolerance) for the origin, none for the waypoints.
bearingsParam :: Int -> Double -> Text
bearingsParam waypointCount heading =
  T.intercalate ";" $ (show (round heading `mod` 360 :: Int) <> "," <> show bearingToleranceDegrees) : replicate waypointCount ""

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

-- | Adds @tollsAvoided@ to OSRM's route response object.
withTollsAvoided :: Bool -> Value -> Value
withTollsAvoided avoided = \case
  A.Object fields -> A.Object (KM.insert "tollsAvoided" (A.Bool avoided) fields)
  other -> other
