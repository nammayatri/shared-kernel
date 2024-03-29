{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Serviceability where

-- import Kernel.Storage.Esqueleto.Config
-- import Kernel.Storage.Esqueleto.SqlDB
-- import Kernel.Storage.Esqueleto.Transactionable

import qualified EulerHS.Language as L
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Geofencing

-- rideServiceable ::
--   ( EsqDBFlow m r,
--     EsqDBReplicaFlow m r
--   ) =>
--   GeofencingConfig ->
--   (LatLong -> [Text] -> SelectSqlDB Bool) ->
--   LatLong ->
--   Maybe LatLong ->
--   m Bool
-- rideServiceable geofencingConfig someGeometriesContain origin mbDestination = do
--   originServiceable <-
--     case geofencingConfig.origin of
--       Unrestricted -> pure True
--       Regions regions -> runInReplica $ someGeometriesContain origin regions
--   destinationServiceable <-
--     case geofencingConfig.destination of
--       Unrestricted -> pure True
--       Regions regions -> do
--         maybe (pure True) (runInReplica . (flip someGeometriesContain) regions) mbDestination
--   pure $ originServiceable && destinationServiceable

rideServiceable ::
  L.MonadFlow m =>
  GeofencingConfig ->
  (LatLong -> [Text] -> m Bool) ->
  LatLong ->
  Maybe LatLong ->
  m Bool
rideServiceable geofencingConfig someGeometriesContain origin mbDestination = do
  originServiceable <-
    case geofencingConfig.origin of
      Unrestricted -> pure True
      Regions regions -> someGeometriesContain origin regions
  destinationServiceable <-
    case geofencingConfig.destination of
      Unrestricted -> pure True
      Regions regions -> do
        maybe (pure True) (`someGeometriesContain` regions) mbDestination
  pure $ originServiceable && destinationServiceable
