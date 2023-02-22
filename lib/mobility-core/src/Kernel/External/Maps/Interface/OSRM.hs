 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.Interface.OSRM
  ( module Reexport,
    callOsrmMatch,
    getDistances,
    getOSRMTable 
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
import Kernel.Types.Common
import qualified Data.List.NonEmpty as NE

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

getDistances ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  OSRMCfg ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances osrmCfg request = do
  let pointsList = OSRM.PointsList $ map getCoordinates (toList request.origins) ++ map getCoordinates (toList request.destinations)
  let sourcesList = OSRM.SourcesList [0 .. (length request.origins - 1)]
  let destinationsList = OSRM.DestinationsList [(length request.origins) .. (length request.origins + length request.destinations - 1)]
  response <- OSRM.callOsrmGetDistancesAPI osrmCfg.osrmUrl pointsList sourcesList destinationsList
  getOSRMTable response request

getOSRMTable ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  OSRM.OSRMTableResponse ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getOSRMTable tableResponse request = do
  let pairOfIndexSource = zip [0 .. (length request.origins - 1)] (NE.toList request.origins)
  let pairOfIndexDestinations = zip [0 .. (length request.destinations - 1)] (NE.toList $ request.destinations)

  pure $
    NE.fromList $
      foldl
        ( \mainList sourcePair ->
            do
              let subList =
                    map
                      ( \destinationPair ->
                          GetDistanceResp
                            { origin = snd sourcePair,
                              destination = snd destinationPair,
                              distance = Meters {getMeters = round $ tableResponse.distances !! fst sourcePair !! fst destinationPair},
                              duration = Seconds {getSeconds = round $ tableResponse.durations !! fst sourcePair !! fst destinationPair},
                              status = "ok"
                            }
                      )
                      pairOfIndexDestinations
              mainList ++ subList
        )
        []
        pairOfIndexSource
