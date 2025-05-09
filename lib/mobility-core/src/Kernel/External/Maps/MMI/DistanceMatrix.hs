{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.MMI.DistanceMatrix where

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

type MMIDistanceMatrixAPI =
  "advancedmaps" :> "v1"
    :> Capture "Authorization" Text
    :> "distance_matrix"
    :> "driving"
    :> Capture "coordinates" Text
    :> QueryParam "rtype" Integer
    :> QueryParam "region" Text
    :> QueryParam "sources" Text
    :> QueryParam "destinations" Text
    :> Get '[JSON] MMI.DistanceMatrixResp

mmiDistanceMatrixAPI :: Proxy MMIDistanceMatrixAPI
mmiDistanceMatrixAPI = Proxy

getDistanceMatrixClient :: Text -> Text -> Maybe Integer -> Maybe Text -> Maybe Text -> Maybe Text -> ET.EulerClient MMI.DistanceMatrixResp
getDistanceMatrixClient = ET.client mmiDistanceMatrixAPI

mmiDistanceMatrix ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    ToJSON a,
    ToJSON b,
    HasKafkaProducer r
  ) =>
  Maybe Text ->
  IT.GetDistancesReq a b ->
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  m MMI.DistanceMatrixResp
mmiDistanceMatrix entityId req url apiKey points srcList destList = do
  rsp <-
    callMMIAPI
      url
      (getDistanceMatrixClient apiKey points (Just 0) (Just "ind") srcList destList)
      "mmi-distance-matrix"
      mmiDistanceMatrixAPI
  fork ("Logging external API Call of mmiDistanceMatrix MMI ") $
    ApiCallLogger.pushExternalApiCallDataToKafkaWithTextEncodedResp "mmiDistanceMatrix" "MMI" entityId (Just req) $ KUT.encodeToText rsp
  return rsp

callMMIAPI :: CallAPI env api a
callMMIAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_NOT_AVAILABLE")
    Nothing
