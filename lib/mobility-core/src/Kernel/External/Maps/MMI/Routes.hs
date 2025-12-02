{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.MMI.Routes where

import Data.Maybe
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.External.Encryption
import Kernel.External.Maps.Interface.Types as IT
import qualified Kernel.External.Maps.MMI.Types as MMI
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Kernel.Utils.ExternalAPICallLogging as ApiCallLogger
import qualified Kernel.Utils.Text as KUT
import Servant hiding (throwError)

type MMIRouteAPI =
  "advancedmaps" :> "v1"
    :> Capture "Authorization" Text
    :> "route_traffic"
    :> "driving"
    :> Capture "cordinates" Text
    :> QueryParam "steps" Bool
    :> QueryParam "region" Text
    :> QueryParam "overview" Text
    :> QueryParam "alternatives" Int
    :> Get '[JSON] MMI.RouteResponse

mmiRouteAPI :: Proxy MMIRouteAPI
mmiRouteAPI = Proxy

getRouteClient :: Text -> Text -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Int -> ET.EulerClient MMI.RouteResponse
getRouteClient = ET.client mmiRouteAPI

mmiRoute ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasKafkaProducer r,
    HasRequestId r,
    MonadReader r m
  ) =>
  Maybe Text ->
  IT.GetRoutesReq ->
  BaseUrl ->
  Text ->
  Text ->
  m MMI.RouteResponse
mmiRoute entityId req url apiKey points = do
  rsp <-
    callMMIAPI
      url
      (getRouteClient apiKey points (Just True) (Just "ind") (Just "full") (Just 3))
      "mmi-route"
      mmiRouteAPI
  fork ("Logging external API Call of mmiRoute MMI ") $
    ApiCallLogger.pushExternalApiCallDataToKafkaWithTextEncodedResp "mmiRoute" "MMI" entityId (Just req) $ KUT.encodeToText rsp
  return rsp

callMMIAPI :: CallAPI m r api a
callMMIAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_NOT_AVAILABLE")
    Nothing
