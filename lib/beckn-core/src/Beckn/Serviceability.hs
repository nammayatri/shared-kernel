module Beckn.Serviceability where

import Beckn.External.Maps.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.Geofencing

rideServiceableDefault ::
  (EsqDBFlow m r, HasField "geofencingConfig" r GeofencingConfig) =>
  (LatLong -> [Text] -> m Bool) ->
  LatLong ->
  Maybe LatLong ->
  m Bool
rideServiceableDefault someGeometriesContain origin mbDestination = do
  geoConfig <- asks (.geofencingConfig)
  rideServiceable geoConfig someGeometriesContain origin mbDestination

rideServiceable ::
  EsqDBFlow m r =>
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
