{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.NextBillion.Route
  ( DirectionsAPI,
    directions,
  )
where

import Data.Text as T
import EulerHS.Types (EulerClient, client)
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.External.Maps.NextBillion.Types as NextBillion
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics.Types (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

type DirectionsAPI =
  "directions" :> "json"
    :> MandatoryQueryParam "origin" Place
    :> MandatoryQueryParam "destination" Place
    :> MandatoryQueryParam "key" Text
    :> QueryParam "waypoints" [Place]
    :> QueryParam "alternatives" Bool
    :> QueryParam "altcount" Int
    :> Get '[JSON] NextBillion.DirectionsResp

directionsClient ::
  Place ->
  Place ->
  Text ->
  Maybe [Place] ->
  Maybe Bool ->
  Maybe Int ->
  EulerClient NextBillion.DirectionsResp
directionsClient = client (Proxy :: Proxy DirectionsAPI)

directions ::
  ( HasCallStack,
    CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Place ->
  Place ->
  Maybe [Place] ->
  m NextBillion.DirectionsResp
directions url key origin destination waypoints = do
  callAPI url (directionsClient origin destination key waypoints (Just True) Nothing) "next-billion-route" (Proxy @DirectionsAPI)
    >>= fromEitherM (FailedToCallNextBillionRouteAPI . show)
