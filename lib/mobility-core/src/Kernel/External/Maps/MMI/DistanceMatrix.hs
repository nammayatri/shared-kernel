module Beckn.External.Maps.MMI.DistanceMatrix where

import Beckn.External.Encryption
import qualified Beckn.External.Maps.MMI.MapsClient.Types as MMI
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
-- import Beckn.External.Maps.MMI.Config
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import Data.Maybe
import EulerHS.Prelude
import qualified EulerHS.Types as ET
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
