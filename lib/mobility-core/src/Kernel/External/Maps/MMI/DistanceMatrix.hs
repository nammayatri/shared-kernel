module Kernel.External.Maps.MMI.DistanceMatrix where

import Data.Maybe
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.External.Encryption
import qualified Kernel.External.Maps.MMI.MapsClient.Types as MMI
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

callMMIAPI :: CallAPI env a
callMMIAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_NOT_AVAILABLE")
