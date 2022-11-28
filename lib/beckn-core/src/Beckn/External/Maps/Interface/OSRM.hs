module Beckn.External.Maps.Interface.OSRM
  ( module Reexport,
    callOsrmMatch,
  )
where

import Beckn.External.Maps.Google.Config as Reexport
import Beckn.External.Maps.Google.PolyLinePoints
import Beckn.External.Maps.HasCoordinates as Reexport (HasCoordinates (..))
import Beckn.External.Maps.Interface.Types
import Beckn.External.Maps.OSRM.Config
import qualified Beckn.External.Maps.OSRM.RoadsClient as OSRM
import Beckn.External.Maps.Types as Reexport
import Beckn.Prelude
import qualified Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.App

callOsrmMatch ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  OSRMCfg ->
  SnapToRoadReq ->
  m SnapToRoadResp
callOsrmMatch osrmCfg (SnapToRoadReq wps) = do
  let mbRadius = fmap (.getMeters) osrmCfg.radiusDeviation
  res <- OSRM.callOsrmMatchAPI osrmCfg.osrmUrl mbRadius (OSRM.PointsList wps)
  (dist, interpolatedPts) <- OSRM.getResultOneRouteExpected res
  pure $ SnapToRoadResp dist interpolatedPts
