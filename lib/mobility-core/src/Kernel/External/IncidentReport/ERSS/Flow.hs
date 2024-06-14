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
  )
where

import EulerHS.Types as Euler
import qualified Kernel.External.IncidentReport.ERSS.Types as ERSS
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (callAPI, fromEitherM)
import Servant hiding (throwError)

type ERSSIncidentReportAPI =
  "interconnect/api/sos/register"
    :> ReqBody '[JSON] ERSS.IncidentReportReq
    :> Post '[JSON] ERSS.IncidentReportRes

reportIncident ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  ERSS.IncidentReportReq ->
  BaseUrl ->
  m ERSS.IncidentReportRes
reportIncident req url = do
  let eulerClient = Euler.client (Proxy @ERSSIncidentReportAPI)
  callAPI url (eulerClient req) "incidentReportAPI" (Proxy @ERSSIncidentReportAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call create ticket API: " <> show err)
