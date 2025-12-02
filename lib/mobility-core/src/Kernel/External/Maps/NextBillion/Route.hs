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
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics.Types (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Kernel.Utils.ExternalAPICallLogging as ApiCallLogger
import Servant hiding (throwError)

type DirectionsAPI =
  "directions" :> "json"
    :> MandatoryQueryParam "origin" Place
    :> MandatoryQueryParam "destination" Place
    :> MandatoryQueryParam "key" Text
    :> QueryParam "waypoints" [Place]
    :> QueryParam "alternatives" Bool
    :> QueryParam "altcount" Int
    :> QueryParam "route_type" Text
    :> QueryParam "option" Text
    :> QueryParam "mode" Text
    :> Get '[JSON] NextBillion.DirectionsResp

directionsClient ::
  Place ->
  Place ->
  Text ->
  Maybe [Place] ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  EulerClient NextBillion.DirectionsResp
directionsClient = client (Proxy :: Proxy DirectionsAPI)

directions ::
  ( HasCallStack,
    CoreMetrics m,
    MonadFlow m,
    ToJSON a,
    MonadReader r m,
    HasKafkaProducer r,
    HasRequestId r
  ) =>
  Maybe Text ->
  a ->
  BaseUrl ->
  Text ->
  Place ->
  Place ->
  Maybe [Place] ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  m NextBillion.DirectionsResp
directions entityId req url key origin destination waypoints alternatives altcount routeType option = do
  rsp <- callAPI url (directionsClient origin destination key waypoints alternatives altcount routeType option (Just "4w")) "next-billion-route" (Proxy @DirectionsAPI)
  fork ("Logging external API Call of directions NextBillion ") $
    ApiCallLogger.pushExternalApiCallDataToKafka "directions" "NextBillion" entityId (Just req) rsp
  fromEitherM (FailedToCallNextBillionRouteAPI . show) rsp
