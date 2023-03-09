{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.MMI.ReverseGeocoding where

import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.External.Encryption
import qualified Kernel.External.Maps.MMI.Types as MMI
import Kernel.External.Maps.Types
import Kernel.External.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant

type MMIReverseGeocodeAPI =
  "advancedmaps" :> "v1"
    :> Capture "Authorization" Text
    :> "rev_geocode"
    :> MandatoryQueryParam "lat" Double
    :> MandatoryQueryParam "lng" Double
    :> QueryParam "region" Text
    :> QueryParam "lang" Language
    :> Get '[JSON] MMI.ReverseGeocodeResp

mmiReverseGeocodeAPI :: Proxy MMIReverseGeocodeAPI
mmiReverseGeocodeAPI = Proxy

getReverseGeocodeClient :: Text -> Double -> Double -> Maybe Text -> Maybe Language -> ET.EulerClient MMI.ReverseGeocodeResp
getReverseGeocodeClient = ET.client mmiReverseGeocodeAPI

mmiReverseGeocode ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  LatLong ->
  Maybe Text ->
  Maybe Language ->
  m MMI.ReverseGeocodeResp
mmiReverseGeocode url apiKey LatLong {..} region lang = do
  callMMIAPI
    url
    (getReverseGeocodeClient apiKey lat lon region lang)
    "mmi-reverse-geocode"

callMMIAPI :: CallAPI env a
callMMIAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_NOT_AVAILABLE")
