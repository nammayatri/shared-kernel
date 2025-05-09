{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.MMI.SnapToRoad where

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

type MMISnapToRoadAPI =
  "advancedmaps" :> "v1"
    :> Capture "Authorization" Text
    :> "snapToRoad"
    :> MandatoryQueryParam "pts" Text
    :> QueryParam "timestamps" Text
    :> QueryParam "geometries" Text
    :> QueryParam "radiuses" Text
    :> QueryParam "region" Text
    :> Get '[JSON] MMI.SnapToRoadResp

mmiSnapToRoadAPI :: Proxy MMISnapToRoadAPI
mmiSnapToRoadAPI = Proxy

getSnapToRoadClient :: Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ET.EulerClient MMI.SnapToRoadResp
getSnapToRoadClient = ET.client mmiSnapToRoadAPI

callMMIAPI :: CallAPI env api a
callMMIAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_NOT_AVAILABLE")
    Nothing

mmiSnapToRoad ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  IT.SnapToRoadReq ->
  BaseUrl ->
  Text ->
  Text ->
  m MMI.SnapToRoadResp
mmiSnapToRoad entityId req url apiKey points = do
  rsp <-
    callMMIAPI
      url
      (getSnapToRoadClient apiKey points Nothing Nothing Nothing (Just "ind"))
      "mmi-snap-to-road"
      mmiSnapToRoadAPI
  fork ("Logging external API Call of mmiSnapToRoad MMI ") $
    ApiCallLogger.pushExternalApiCallDataToKafkaWithTextEncodedResp "mmiSnapToRoad" "MMI" entityId (Just req) $ KUT.encodeToText rsp
  return rsp
