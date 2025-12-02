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
    reportIncidentUpdate,
  )
where

import Kernel.External.Encryption
import Kernel.External.IncidentReport.ERSS.Config
import qualified Kernel.External.IncidentReport.ERSS.Flow as EF
import Kernel.External.IncidentReport.ERSS.Types as ERSS
import qualified Kernel.External.IncidentReport.Interface.Types as Interface
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Servant.Client

reportIncident ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  ERSSCfg ->
  Interface.IncidentReportReq ->
  m Interface.IncidentReportRes
reportIncident config req = do
  erssRequest <- makeERSSRequest req config
  erssResponse <- EF.reportIncident erssRequest config.url req.token
  pure $ makeInterfaceResponse erssResponse
  where
    makeERSSRequest :: EncFlow m r => Interface.IncidentReportReq -> ERSSCfg -> m ERSS.IncidentReportReq
    makeERSSRequest Interface.IncidentReportReq {..} cfg = do
      pure $
        ERSS.IncidentReportReq
          { userName = cfg.userName,
            ..
          }
    makeInterfaceResponse :: ERSS.IncidentReportRes -> Interface.IncidentReportRes
    makeInterfaceResponse ERSS.IncidentReportRes {..} = Interface.IncidentReportRes {incidentData = cast incidentData, ..}
      where
        cast :: ERSS.IncidentReportData -> Interface.IncidentReportData
        cast ERSS.IncidentReportData {..} = Interface.ReportData {..}

reportIncidentUpdate ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  ERSSCfg ->
  Interface.IncidentReportUpdateReq ->
  m Interface.IncidentReportUpdateRes
reportIncidentUpdate config req = do
  erssRequest <- makeERSSUpdateRequest req
  erssResponse <- EF.reportIncidentUpdate erssRequest config.url req.token
  pure $ makeInterfaceUpdateResponse erssResponse
  where
    makeERSSUpdateRequest :: EncFlow m r => Interface.IncidentReportUpdateReq -> m ERSS.IncidentReportUpdateReq
    makeERSSUpdateRequest Interface.IncidentReportUpdateReq {..} = pure ERSS.IncidentReportUpdateReq {..}
    makeInterfaceUpdateResponse :: ERSS.IncidentReportUpdateRes -> Interface.IncidentReportUpdateRes
    makeInterfaceUpdateResponse ERSS.IncidentReportUpdateRes {..} = Interface.IncidentReportUpdateRes {..}
