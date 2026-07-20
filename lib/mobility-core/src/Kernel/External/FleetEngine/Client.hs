{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.FleetEngine.Client
  ( defaultFleetEngineBaseUrl,
    createTrip,
    updateTrip,
    updateTripStatus,
    assignVehicleAndStart,
    createVehicle,
    getVehicle,
  )
where

import qualified Data.Aeson as A
import qualified Data.Text as T
import EulerHS.Types (EulerClient, client)
import Kernel.External.FleetEngine.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common
import Servant hiding (throwError)
import Servant.Client (Scheme (..))

type CreateTripAPI =
  "v1"
    :> "providers"
    :> Capture "providerId" Text
    :> "trips"
    :> MandatoryQueryParam "tripId" Text
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Trip
    :> Post '[JSON] A.Value

type UpdateTripAPI =
  "v1"
    :> "providers"
    :> Capture "providerId" Text
    :> "trips"
    :> Capture "tripId" Text
    :> MandatoryQueryParam "updateMask" Text
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Trip
    :> Put '[JSON] A.Value

type CreateVehicleAPI =
  "v1"
    :> "providers"
    :> Capture "providerId" Text
    :> "vehicles"
    :> MandatoryQueryParam "vehicleId" Text
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Vehicle
    :> Post '[JSON] A.Value

type GetVehicleAPI =
  "v1"
    :> "providers"
    :> Capture "providerId" Text
    :> "vehicles"
    :> Capture "vehicleId" Text
    :> Header "Authorization" Text
    :> Get '[JSON] Vehicle

defaultFleetEngineBaseUrl :: BaseUrl
defaultFleetEngineBaseUrl =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "fleetengine.googleapis.com",
      baseUrlPort = 443,
      baseUrlPath = ""
    }

createTripClient :: Text -> Text -> Maybe Text -> Trip -> EulerClient A.Value
createTripClient = client (Proxy :: Proxy CreateTripAPI)

updateTripClient :: Text -> Text -> Text -> Maybe Text -> Trip -> EulerClient A.Value
updateTripClient = client (Proxy :: Proxy UpdateTripAPI)

createVehicleClient :: Text -> Text -> Maybe Text -> Vehicle -> EulerClient A.Value
createVehicleClient = client (Proxy :: Proxy CreateVehicleAPI)

getVehicleClient :: Text -> Text -> Maybe Text -> EulerClient Vehicle
getVehicleClient = client (Proxy :: Proxy GetVehicleAPI)

bearer :: Text -> Maybe Text
bearer token = Just ("Bearer " <> token)

-- | Create a Fleet Engine trip. @tripId@ is the (1:1) BPP ride id, which makes
-- this idempotent: a re-issued CreateTrip for an existing trip returns
-- ALREADY_EXISTS and is treated as success.
createTrip ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasRequestId r) =>
  BaseUrl ->
  Text -> -- providerId
  Text -> -- token (server JWT)
  Text -> -- tripId
  Trip ->
  m ()
createTrip baseUrl providerId token tripId trip = do
  result <-
    callAPI
      baseUrl
      (createTripClient providerId tripId (bearer token) trip)
      "fleetEngineCreateTrip"
      (Proxy :: Proxy CreateTripAPI)
  case result of
    Right _ -> logInfo $ "FleetEngine: created trip " <> tripId
    Left err
      | "ALREADY_EXISTS" `T.isInfixOf` show err ->
        logInfo $ "FleetEngine: trip already exists (idempotent no-op) " <> tripId
      | otherwise -> logError $ "FleetEngine: createTrip failed for " <> tripId <> ": " <> show err

updateTrip ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasRequestId r) =>
  BaseUrl ->
  Text -> -- providerId
  Text -> -- token (server JWT)
  Text -> -- tripId
  Text -> -- updateMask (comma-separated field paths)
  Trip ->
  m ()
updateTrip baseUrl providerId token tripId updateMask trip = do
  result <-
    callAPI
      baseUrl
      (updateTripClient providerId tripId updateMask (bearer token) trip)
      "fleetEngineUpdateTrip"
      (Proxy :: Proxy UpdateTripAPI)
  case result of
    Right _ -> logInfo $ "FleetEngine: updated trip " <> tripId <> " [" <> updateMask <> "]"
    Left err -> logError $ "FleetEngine: updateTrip failed for " <> tripId <> ": " <> show err

-- | Convenience: advance a trip's status.
updateTripStatus ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasRequestId r) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  TripStatus ->
  m ()
updateTripStatus baseUrl providerId token tripId status =
  updateTrip baseUrl providerId token tripId "tripStatus" (emptyTrip {tripStatus = Just status})

-- Fleet Engine requires at least one successful CreateVehicle per provider
-- before any Trips API works (project provisioning); ALREADY_EXISTS is
-- treated as success.
createVehicle ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasRequestId r) =>
  BaseUrl ->
  Text -> -- providerId
  Text -> -- token (server JWT)
  Text -> -- vehicleId
  Vehicle ->
  m ()
createVehicle baseUrl providerId token vehicleId vehicle = do
  result <-
    callAPI
      baseUrl
      (createVehicleClient providerId vehicleId (bearer token) vehicle)
      "fleetEngineCreateVehicle"
      (Proxy :: Proxy CreateVehicleAPI)
  case result of
    Right _ -> logInfo $ "FleetEngine: created vehicle " <> vehicleId
    Left err
      | "ALREADY_EXISTS" `T.isInfixOf` show err ->
        logInfo $ "FleetEngine: vehicle already exists (idempotent no-op) " <> vehicleId
      | otherwise -> logError $ "FleetEngine: createVehicle failed for " <> vehicleId <> ": " <> show err

-- 'Nothing' on NOT_FOUND (for get-or-create); other errors also collapse to
-- 'Nothing', matching the log-and-continue style used here.
getVehicle ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasRequestId r) =>
  BaseUrl ->
  Text -> -- providerId
  Text -> -- token (server JWT)
  Text -> -- vehicleId
  m (Maybe Vehicle)
getVehicle baseUrl providerId token vehicleId = do
  result <-
    callAPI
      baseUrl
      (getVehicleClient providerId vehicleId (bearer token))
      "fleetEngineGetVehicle"
      (Proxy :: Proxy GetVehicleAPI)
  case result of
    Right v -> pure (Just v)
    Left err
      | "NOT_FOUND" `T.isInfixOf` show err -> pure Nothing
      | otherwise -> do
        logError $ "FleetEngine: getVehicle failed for " <> vehicleId <> ": " <> show err
        pure Nothing

-- | Convenience: assign the vehicle to the trip and move it to ENROUTE_TO_PICKUP.
assignVehicleAndStart ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasRequestId r) =>
  BaseUrl ->
  Text ->
  Text ->
  Text -> -- tripId
  Text -> -- vehicleId
  m ()
assignVehicleAndStart baseUrl providerId token tripId vehicleId =
  updateTrip
    baseUrl
    providerId
    token
    tripId
    "tripStatus,vehicleId"
    (emptyTrip {tripStatus = Just ENROUTE_TO_PICKUP, vehicleId = Just vehicleId})
