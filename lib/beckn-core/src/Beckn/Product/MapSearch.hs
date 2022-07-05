module Beckn.Product.MapSearch where

import Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Prelude
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common hiding (id)

getDistance ::
  ( MonadFlow m,
    CoreMetrics m,
    HasGoogleMaps m r,
    GoogleMaps.HasCoordinates a,
    GoogleMaps.HasCoordinates b
  ) =>
  Maybe MapSearch.TravelMode ->
  a ->
  b ->
  m (GoogleMaps.GetDistanceResult a b)
getDistance travelMode origin destination = do
  key <- asks (.googleMapsKey)
  case key of
    "mock-key" -> pure $ makeMockGetDistanceResult origin destination
    _ -> GoogleMaps.getDistance travelMode origin destination Nothing

getDistances ::
  ( MonadFlow m,
    CoreMetrics m,
    HasGoogleMaps m r,
    GoogleMaps.HasCoordinates a,
    GoogleMaps.HasCoordinates b
  ) =>
  Maybe MapSearch.TravelMode ->
  NonEmpty a ->
  NonEmpty b ->
  m (NonEmpty (GoogleMaps.GetDistanceResult a b))
getDistances travelMode origins destinations = do
  key <- asks (.googleMapsKey)
  case key of
    "mock-key" -> pure $ makeMockGetDistanceResult <$> origins <*> destinations
    _ -> GoogleMaps.getDistances travelMode origins destinations Nothing

-- FIXME Should we use some calculation here?

makeMockGetDistanceResult ::
  ( GoogleMaps.HasCoordinates a,
    GoogleMaps.HasCoordinates b
  ) =>
  a ->
  b ->
  GoogleMaps.GetDistanceResult a b
makeMockGetDistanceResult origin dest =
  GoogleMaps.GetDistanceResult
    { origin = origin,
      destination = dest,
      distance = Meters 9446,
      duration = mockDuration,
      duration_in_traffic = mockDuration,
      status = "OK"
    }
  where
    mockDuration = 648
