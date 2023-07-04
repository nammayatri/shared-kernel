{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Serviceability where

import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Esqueleto.SqlDB
import Kernel.Storage.Esqueleto.Transactionable
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error (ServiceabilityError (..))
import Kernel.Types.Geofencing
import Kernel.Utils.Common

rideServiceable ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  GeofencingConfig ->
  (LatLong -> [Text] -> SelectSqlDB (Maybe (Context.City, Context.Country))) ->
  LatLong ->
  Maybe LatLong ->
  m (Context.City, Context.Country)
rideServiceable geofencingConfig someGeometriesContain origin mbDestination = do
  case (geofencingConfig.origin, geofencingConfig.destination) of
    (Regions originRegions, Regions destRegions) -> do
      (originCity, originCountry) <- runInReplica $ someGeometriesContain origin originRegions >>= fromMaybeM RideNotServiceable
      (destCity, _destCountry) <- maybe (pure (originCity, originCountry)) (\destination -> runInReplica $ someGeometriesContain destination destRegions >>= fromMaybeM RideNotServiceable) mbDestination
      if originCity == destCity
        then return (originCity, originCountry)
        else throwError $ OriginAndDestinationCityMismatch (show originCity) (show destCity)
    (_, _) -> throwError RideNotServiceable
