module Kernel.External.Maps.MMI.Routes where

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

type MMIRouteAPI =
  "advancedmaps" :> "v1"
    :> Capture "Authorization" Text
    :> "route_eta"
    :> "driving"
    :> Capture "cordinates" Text
    :> QueryParam "steps" Bool
    :> QueryParam "region" Text
    :> QueryParam "overview" Text
    :> Get '[JSON] MMI.RouteResponse

mmiRouteAPI :: Proxy MMIRouteAPI
mmiRouteAPI = Proxy

getRouteClient :: Text -> Text -> Maybe Bool -> Maybe Text -> Maybe Text -> ET.EulerClient MMI.RouteResponse
getRouteClient = ET.client mmiRouteAPI

mmiRoute ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  m MMI.RouteResponse
mmiRoute url apiKey points = do
  callMMIAPI
    url
    (getRouteClient apiKey points (Just True) (Just "ind") (Just "full"))
    "mmi-route"

callMMIAPI :: CallAPI env a
callMMIAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_NOT_AVAILABLE")
