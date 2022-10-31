module Beckn.External.Maps.Interface
  ( module Reexport,
    getDistance,
    getDistances,
    getRoutes,
    snapToRoad,
    autoComplete,
    getPlaceDetails,
    getPlaceName,
  )
where

import Beckn.External.Maps.Google.Config as Reexport
import Beckn.External.Maps.HasCoordinates as Reexport (HasCoordinates (..))
import qualified Beckn.External.Maps.Interface.Google as Google
import Beckn.External.Maps.Interface.Types as Reexport
import Beckn.External.Maps.Types as Reexport
import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import Beckn.Utils.Common hiding (id)

getDistance ::
  ( MonadFlow m,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  MapsServiceConfig ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistance serviceConfig GetDistanceReq {..} =
  getDistances serviceConfig getDistancesReq >>= \case
    (a :| []) -> return a
    _ -> throwError (InternalError "Exactly one getDistance result expected.")
  where
    getDistancesReq =
      GetDistancesReq
        { origins = origin :| [],
          destinations = destination :| [],
          ..
        }

getDistances ::
  ( MonadFlow m,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  MapsServiceConfig ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getDistances cfg req

getRoutes ::
  ( MonadFlow m,
    CoreMetrics m,
    Log m
  ) =>
  MapsServiceConfig ->
  GetRoutesReq ->
  m GetRoutesResp
getRoutes serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getRoutes cfg req

snapToRoad ::
  ( HasCallStack,
    CoreMetrics m,
    MonadFlow m
  ) =>
  MapsServiceConfig ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.snapToRoad cfg req

autoComplete ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  MapsServiceConfig ->
  AutoCompleteReq ->
  m AutoCompleteResp
autoComplete serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.autoComplete cfg req

getPlaceDetails ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  MapsServiceConfig ->
  GetPlaceDetailsReq ->
  m GetPlaceDetailsResp
getPlaceDetails serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getPlaceDetails cfg req

getPlaceName ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  MapsServiceConfig ->
  GetPlaceNameReq ->
  m GetPlaceNameResp
getPlaceName serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getPlaceName cfg req
