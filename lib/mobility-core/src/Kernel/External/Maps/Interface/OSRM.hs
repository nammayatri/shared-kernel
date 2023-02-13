module Kernel.External.Maps.Interface.OSRM
  ( module Reexport,
    callOsrmMatch,
  )
where

import Kernel.External.Maps.Google.Config as Reexport
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.External.Maps.HasCoordinates as Reexport (HasCoordinates (..))
import Kernel.External.Maps.Interface.Types
import Kernel.External.Maps.OSRM.Config
import qualified Kernel.External.Maps.OSRM.RoadsClient as OSRM
import Kernel.External.Maps.Types as Reexport
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.App

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
