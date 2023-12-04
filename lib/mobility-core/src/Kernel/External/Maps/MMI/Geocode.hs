{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Kernel.External.Maps.MMI.Geocode where

import Data.Maybe
import EulerHS.Prelude
import EulerHS.Types as ET
import qualified Kernel.External.Maps.MMI.Types as MMI
import Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant

type MMIGeocodeAPI =
  "api" :> "places" :> "geocode"
    :> Header "Authorization" MMI.MMIAuthToken
    :> MandatoryQueryParam "address" Text
    -- :> QueryParam "itemCount" Int     -- Uncomment if array of response is expected in copResults of geocode response
    :> QueryParam "region" Text
    :> Get '[JSON] MMI.GeocodeResp

mmiGeocodeAPI :: Proxy MMIGeocodeAPI
mmiGeocodeAPI = Proxy

mmiGeocodeClient :: Maybe MMI.MMIAuthToken -> Text -> Maybe Text -> ET.EulerClient MMI.GeocodeResp
mmiGeocodeClient = ET.client mmiGeocodeAPI

mmiGeoCode ::
  ( CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  BaseUrl ->
  Maybe MMI.MMIAuthToken ->
  Text ->
  m MMI.GeocodeResp
mmiGeoCode url authToken address = do
  callMMIGeocodeAPI
    url
    (mmiGeocodeClient authToken address (Just "ind"))
    "mmi-auto-suggest"
    mmiGeocodeAPI

callMMIGeocodeAPI :: CallAPI env api a
callMMIGeocodeAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_GEOCODE_ERROR")
    Nothing
