{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.IncidentReport.ERSS.Flow
  ( reportIncident,
    reportIncidentUpdate,
  )
where

import qualified EulerHS.Types as ET
import qualified Kernel.External.IncidentReport.ERSS.Types as ERSS
import Kernel.External.IncidentReport.Interface.Error
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common (callAPI, fromEitherM)
import Servant hiding (throwError)

type ERSSIncidentReportAPI =
  "WBJMONRestAPI" :> "rs" :> "v1" :> "master" :> "saveInitiateJourneyInfo_v1"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] ERSS.IncidentReportReq
    :> Post '[JSON] ERSS.IncidentReportRes

reportIncident ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  ERSS.IncidentReportReq ->
  BaseUrl ->
  Text ->
  m ERSS.IncidentReportRes
reportIncident req url token = do
  let eulerClient = ET.client (Proxy @ERSSIncidentReportAPI)
  callAPI url (eulerClient (Just $ "Bearer " <> token) req) "incidentReportAPI" (Proxy @ERSSIncidentReportAPI)
    >>= fromEitherM (FailedToCallIncidentReportAPI . show)

type ERSSIncidentReportUpdateAPI =
  "WBJMONRestAPI" :> "rs" :> "v1" :> "master" :> "updateJourneyLocation_v1"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] ERSS.IncidentReportUpdateReq
    :> Post '[JSON] ERSS.IncidentReportUpdateRes

reportIncidentUpdate ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  ERSS.IncidentReportUpdateReq ->
  BaseUrl ->
  Text ->
  m ERSS.IncidentReportUpdateRes
reportIncidentUpdate req url token = do
  let eulerClient = ET.client (Proxy @ERSSIncidentReportUpdateAPI)
  callAPI url (eulerClient (Just $ "Bearer " <> token) req) "incidentReportUpdateAPI" (Proxy @ERSSIncidentReportUpdateAPI)
    >>= fromEitherM (FailedToCallIncidentReportUpdateAPI . show)
