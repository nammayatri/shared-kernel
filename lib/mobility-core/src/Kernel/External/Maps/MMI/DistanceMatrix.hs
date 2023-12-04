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
import qualified Kernel.External.Maps.MMI.Types as MMI
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
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
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  m MMI.DistanceMatrixResp
mmiDistanceMatrix url apiKey points srcList destList = do
  callMMIAPI
    url
    (getDistanceMatrixClient apiKey points (Just 0) (Just "ind") srcList destList)
    "mmi-distance-matrix"
    mmiDistanceMatrixAPI

callMMIAPI :: CallAPI env api a
callMMIAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_NOT_AVAILABLE")
    Nothing
