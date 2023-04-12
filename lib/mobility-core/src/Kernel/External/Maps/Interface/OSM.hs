{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.Interface.OSM where

import Kernel.External.Maps.Interface.Types
import Kernel.External.Maps.OSM.Config
import qualified Kernel.External.Maps.OSM.ReverseGeocode as OSM
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App

getPlaceDetailsFromLatLon ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  OSMCfg ->
  GetPlaceDetailsFromLatLonReq ->
  m GetPlaceDetailsFromLatLonResp
getPlaceDetailsFromLatLon osmCfg req = do
  reverseGeocodeRes <- OSM.getPlaceDetailsFromLatLon osmCfg.osmUrl req.responseFormat req.location.lat req.location.lon
  return
    GetPlaceDetailsFromLatLonResp
      { placeId = reverseGeocodeRes.place_id,
        displayName = reverseGeocodeRes.display_name,
        address =
          PlaceDetails
            { city = reverseGeocodeRes.address.state_district,
              state_ = reverseGeocodeRes.address.state,
              country = reverseGeocodeRes.address.country,
              street = reverseGeocodeRes.address.road,
              building = Nothing,
              areaCode = reverseGeocodeRes.address.postcode,
              area = reverseGeocodeRes.address.neighbourhood
            }
      }
