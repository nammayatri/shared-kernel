{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.IncidentReport.Interface.ERSS
  ( reportIncident,
  )
where

import Kernel.External.Encryption
import Kernel.External.IncidentReport.ERSS.Config
import qualified Kernel.External.IncidentReport.ERSS.Flow as EF
import Kernel.External.IncidentReport.ERSS.Types as ERSS
import qualified Kernel.External.IncidentReport.Interface.Types as IT
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics

reportIncident ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  ERSSCfg ->
  IT.IncidentReportReq ->
  m IT.IncidentReportRes
reportIncident config req = do
  request <- mkERSSReq req config
  resp <- EF.reportIncident request config.url
  pure $ transformERSSResp resp

transformERSSResp :: ERSS.IncidentReportRes -> IT.IncidentReportRes
transformERSSResp resp = case resp.resultCode of
  "COMM_OPERATION_SUCCESS" -> IT.Success
  "COMM_UNAUTHORIZED_TOKEN" -> IT.Fail
  _ -> IT.Unknown

mkERSSReq ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  IT.IncidentReportReq ->
  ERSSCfg ->
  m ERSS.IncidentReportReq
mkERSSReq IT.IncidentReportReq {..} cfg = do
  authId <- decrypt cfg.authId
  authCode <- decrypt cfg.authCode
  pure
    ERSS.IncidentReportReq
      { tripInfo = getTripInfo <$> tripInfo,
        routeMap = getRouteMap,
        ..
      }
  where
    getTripInfo IT.TripInfo {..} = ERSS.TripInfo {..}
    getRouteMap =
      ( map
          ( \point ->
              ERSS.RouteMap
                { latitude = point.latitude,
                  longitude = point.longitude
                }
          )
      )
        <$> routeMap
