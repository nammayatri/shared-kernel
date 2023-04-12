{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.Interface
  ( module Reexport,
    getDistance,
    getDistancesProvided,
    getDistances,
    getRoutesProvided,
    getRoutes,
    snapToRoadProvided,
    snapToRoad,
    autoCompleteProvided,
    autoComplete,
    getPlaceDetailsProvided,
    getPlaceDetails,
    getPlaceNameProvided,
    getPlaceName,
    getPlaceDetailsFromLatLonProvided,
    getPlaceDetailsFromLatLon,
  )
where

import EulerHS.Prelude ((...))
import Kernel.External.Maps.Google.Config as Reexport
import Kernel.External.Maps.HasCoordinates as Reexport (HasCoordinates (..))
import qualified Kernel.External.Maps.Interface.Google as Google
import qualified Kernel.External.Maps.Interface.MMI as MMI
import qualified Kernel.External.Maps.Interface.OSM as OSM
import qualified Kernel.External.Maps.Interface.OSRM as OSRM
import Kernel.External.Maps.Interface.Types as Reexport
import Kernel.External.Maps.MMI.Config as Reexport
import Kernel.External.Maps.OSRM.Config as Reexport
import Kernel.External.Maps.Types as Reexport
import Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Utils.Common hiding (id)

getDistance ::
  ( EncFlow m r,
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

mkNotProvidedError :: Text -> MapsService -> Text
mkNotProvidedError functionName serviceName = "Function " <> functionName <> " is not provided by service " <> show serviceName

throwNotProvidedError :: (MonadFlow m) => Text -> MapsService -> m a
throwNotProvidedError =
  (throwError . InternalError) ... mkNotProvidedError

getDistancesProvided :: MapsService -> Bool
getDistancesProvided = \case
  Google -> True
  OSRM -> False
  MMI -> True
  OSM -> False

-- FIXME this logic is redundant, because we throw error always when getDistancesProvided service = False
getDistances ::
  ( EncFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  MapsServiceConfig ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getDistances cfg req
  OSRMConfig cfg -> OSRM.getDistances cfg req
  MMIConfig cfg -> MMI.getDistanceMatrix cfg req
  OSMConfig _ -> throwNotProvidedError "autoComplete" OSM

getRoutesProvided :: MapsService -> Bool
getRoutesProvided = \case
  Google -> True
  OSRM -> False
  MMI -> False
  OSM -> False

getRoutes ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m
  ) =>
  MapsServiceConfig ->
  GetRoutesReq ->
  m GetRoutesResp
getRoutes serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getRoutes cfg req
  OSRMConfig osrmCfg -> OSRM.getRoutes osrmCfg req
  MMIConfig cfg -> MMI.getRoutes cfg req
  OSMConfig _ -> throwNotProvidedError "getRoutes" OSM

snapToRoadProvided :: MapsService -> Bool
snapToRoadProvided = \case
  Google -> True
  OSRM -> True
  MMI -> False
  OSM -> False

snapToRoad ::
  ( EncFlow m r,
    CoreMetrics m,
    HasField "snapToRoadSnippetThreshold" r HighPrecMeters
  ) =>
  MapsServiceConfig ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.snapToRoad cfg req
  OSRMConfig osrmCfg -> OSRM.callOsrmMatch osrmCfg req
  MMIConfig _ -> throwNotProvidedError "snapToRoad" MMI
  OSMConfig _ -> throwNotProvidedError "snapToRoad" OSM

autoCompleteProvided :: MapsService -> Bool
autoCompleteProvided = \case
  Google -> True
  OSRM -> False
  MMI -> True
  OSM -> False

autoComplete ::
  ( EncFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m
  ) =>
  MapsServiceConfig ->
  AutoCompleteReq ->
  m AutoCompleteResp
autoComplete serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.autoComplete cfg req
  OSRMConfig _ -> throwNotProvidedError "autoComplete" OSRM
  MMIConfig cfg -> MMI.autoSuggest cfg req
  OSMConfig _ -> throwNotProvidedError "autoComplete" OSM

getPlaceDetailsProvided :: MapsService -> Bool
getPlaceDetailsProvided = \case
  Google -> True
  OSRM -> False
  MMI -> False
  OSM -> False

getPlaceDetails ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  MapsServiceConfig ->
  GetPlaceDetailsReq ->
  m GetPlaceDetailsResp
getPlaceDetails serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getPlaceDetails cfg req
  OSRMConfig _ -> throwNotProvidedError "getPlaceDetails" OSRM
  MMIConfig _ -> throwNotProvidedError "getPlaceDetails" MMI
  OSMConfig _ -> throwNotProvidedError "getPlaceDetails" OSM

getPlaceNameProvided :: MapsService -> Bool
getPlaceNameProvided = \case
  Google -> True
  OSRM -> False
  MMI -> False
  OSM -> False

getPlaceName ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  MapsServiceConfig ->
  GetPlaceNameReq ->
  m GetPlaceNameResp
getPlaceName serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getPlaceName cfg req
  OSRMConfig _ -> throwNotProvidedError "getPlaceName" OSRM
  MMIConfig _ -> throwNotProvidedError "getPlaceName" MMI
  OSMConfig _ -> throwNotProvidedError "getPlaceName" OSM

getPlaceDetailsFromLatLonProvided :: MapsService -> Bool
getPlaceDetailsFromLatLonProvided = \case
  Google -> False
  OSRM -> False
  MMI -> False
  OSM -> True

getPlaceDetailsFromLatLon ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  MapsServiceConfig ->
  GetPlaceDetailsFromLatLonReq ->
  m GetPlaceDetailsFromLatLonResp
getPlaceDetailsFromLatLon serviceConfig req = case serviceConfig of
  GoogleConfig _ -> throwNotProvidedError "getPlaceDetailsFromLatLon" OSM
  OSRMConfig _ -> throwNotProvidedError "getPlaceDetailsFromLatLon" OSRM
  MMIConfig _ -> throwNotProvidedError "getPlaceDetailsFromLatLon" MMI
  OSMConfig cfg -> OSM.getPlaceDetailsFromLatLon cfg req
