module Beckn.External.Maps.MMI.Routes where

import Beckn.External.Encryption
import qualified Beckn.External.Maps.MMI.MapsClient.Types as MMI
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import Data.Maybe
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant hiding (throwError)

type MMIRouteAPI =
  "advancedmaps" :> "v1"
    :> Capture "Authorization" Text
    :> "route_eta"
    :> "driving"
    :> Capture "cordinates" Text
    :> QueryParam "steps" Bool
    :> QueryParam "region" Text
    :> Get '[JSON] MMI.RouteResponse

mmiRouteAPI :: Proxy MMIRouteAPI
mmiRouteAPI = Proxy

getRouteClient :: Text -> Text -> Maybe Bool -> Maybe Text -> ET.EulerClient MMI.RouteResponse
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
    (getRouteClient apiKey points (Just True) (Just "ind"))
    "mmi-route"

callMMIAPI :: CallAPI env a
callMMIAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_NOT_AVAILABLE")