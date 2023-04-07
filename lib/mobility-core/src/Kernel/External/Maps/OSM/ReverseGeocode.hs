{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.OSM.ReverseGeocode where

import EulerHS.Types (EulerClient, client)
import Kernel.External.Maps.OSM.MapsClient.Types as OSM
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

type OSMReverseGeocodeAPI =
  "reverse"
    :> MandatoryQueryParam "format" Text
    :> MandatoryQueryParam "lat" Double
    :> MandatoryQueryParam "lon" Double
    :> Get '[JSON] OSM.ReverseGeocodeRes

osmReverseGeocodeAPI :: Proxy OSMReverseGeocodeAPI
osmReverseGeocodeAPI = Proxy

getPlaceDetailsFromLatLonClient :: Text -> Double -> Double -> EulerClient OSM.ReverseGeocodeRes
getPlaceDetailsFromLatLonClient = client osmReverseGeocodeAPI

getPlaceDetailsFromLatLon ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Double ->
  Double ->
  m OSM.ReverseGeocodeRes
getPlaceDetailsFromLatLon osmUrl format latitude longitude = do
  callAPI osmUrl (getPlaceDetailsFromLatLonClient format latitude longitude) "osm-getPlaceDetails"
    >>= fromEitherM (\err -> InternalError $ "Failed to call OSM Reverse Geocode API" <> show err)
